defrecord Genomu.Storage.Memory, staging: nil, log: nil, commits: nil do
  record_type staging: :ets.tid, log: :ets.tid, commits: :ets.tid
end

defimpl Genomu.Storage, for: Genomu.Storage.Memory do
  alias Genomu.Storage.Memory, as: T
  alias :ets, as: ETS

  @object_page_size 32

  @nil_value MsgPack.pack(nil)

  def init(T[] = t, _options) do
     log = ETS.new(__MODULE__, [:ordered_set])
     staging = ETS.new(__MODULE__.Staging, [:ordered_set])
     commits = ETS.new(__MODULE__.Commit, [])
     {:ok, t.update(staging: staging, log: log, commits: commits)}
  end

  def lookup(T[log: log], {key, nil}) do
     case ETS.lookup(log, key) do
       [] -> page = 0
       [{_, page}] -> :ok
     end
     case ETS.lookup(log, {key, page}) do
       [{_, {_, {value, [{clock, txn_clock}|_history]}}}] -> {value, clock, txn_clock}
       [{_, {_, {value, [clock|_history]}}}] -> {value, clock, clock}
       _ -> {@nil_value, "", ""}
     end
  end

  def lookup(T[staging: staging] = t, {_key, rev} = cell) do
    case ETS.lookup(staging, cell) do
      [{^cell, {value, _operation}}] -> {value, rev, rev}
      [] ->
        lookup_cell_txn(t, cell)
    end
  end

  defp lookup_cell_txn(T[log: log] = t, {key, _} = cell) do
     case ETS.lookup(log, key) do
       [] -> page = 0
       [{_, page}] -> :ok
     end
     lookup_cell_txn(t, cell, page)
   end
  defp lookup_cell_txn(T[log: log, staging: staging] = t, {key, rev} = cell, page) do
    case ETS.lookup(log, {key, page}) do
      [] ->
        {@nil_value, "", ""}
      [{_key, {_, {value, [{entry_clock, ^rev}|_]}}}] ->
        {value, entry_clock, rev}
      [{_key, {_, {value, [^rev|_]}}}] ->
        {value, rev, rev}
      [{_key, {value, {_, history}}}] ->
        case Enum.find(history, fn(c) ->
                                    case c do
                                      {entry_clock, _} -> :ok
                                      entry_clock -> :ok
                                    end
                                    entry_clock == rev or
                                    ITC.decode(entry_clock) |>
                                    ITC.le(ITC.decode(rev))
                                end) do
          {entry_clock, txn_rev} ->
            [{_, {value, _operation}}] = ETS.lookup(staging, {key, entry_clock})
            {value, entry_clock, txn_rev}
          txn_rev when is_binary(txn_rev) ->
            {value, txn_rev, txn_rev}
          nil ->
            if page == 0 do
              {@nil_value, "", ""}
            else
              lookup_cell_txn(t, cell, page - 1)
            end
        end
    end
  end

  def stage(T[staging: staging], cell, operation, value) do
     ETS.insert(staging, {cell, {value, operation}})
  end

  def commit(T[staging: staging, log: log, commits: commits], revision, entries, txn_log, commit_object) do
    lc {key, entry_clock} inlist entries do
      case ETS.lookup(log, key) do
        [] -> 
          page = 0
          case ETS.lookup(log, {key, page}) do
            [] ->  history = []; value = @nil_value ; page_ctr = 0
            [{_, {page_ctr, {value, history}}}] -> :ok
          end
        [{_, page}] ->
          [{_, {page_ctr, {value, history}}}] = ETS.lookup(log, {key, page})
      end
      {value, updates} = recalculate(txn_log, key, value, [], staging)
      ETS.insert(staging, updates)
      history_entry = history_entry(entry_clock, revision)
      if page_ctr <= @object_page_size do
        ETS.insert(log, {{key, page}, {page_ctr + 1, {value, [history_entry|history]}}})
      else
        # allocate a new page
        ETS.insert(log, [{key, page + 1},
                         {{key, page + 1}, {1, {value, [history_entry]}}}])
      end
    end
    ETS.insert(commits, {{:C, revision}, {commit_object, [revision]}})
  end


  defp recalculate([], _, acc, updates, _staging), do: {acc, updates}
  defp recalculate([{k, _} = cell|t], k, acc, updates, staging) do
    [{_, {_, op}}] = ETS.lookup(staging, cell)
    acc = Genomu.VNode.apply_operation(op, acc)
    recalculate(t, k, acc, [{cell, {acc, op}}|updates], staging)
  end
  defp recalculate([_|t], k, acc, updates, staging) do
    recalculate(t, k, acc, updates, staging)
  end

  @compile {:inline, [history_entry: 2]}
  defp history_entry(clock, clock), do: clock
  defp history_entry(entry_clock, clock) do
    {entry_clock, clock}
  end

  def delete(T[staging: staging, log: log, commits: commits]) do
    unless ETS.info(staging) == :undefined, do:  ETS.delete(staging)
    unless ETS.info(log) == :undefined, do: ETS.delete(log)
    unless ETS.info(commits) == :undefined, do: ETS.delete(commits)
  end

  def size(T[staging: staging, log: log, commits: commits]) do
    ETS.info(staging)[:size] +
    ETS.info(log)[:size] +
    ETS.info(commits)[:size]
  end

  def reduce(T[staging: staging, log: log, commits: commits], acc0, f) do
     acc = ETS.foldl(fn({k,v}, a) -> f.(k, v, a) end, acc0, log)
     acc = ETS.foldl(fn({k,v}, a) -> f.({:s, k}, v, a) end, acc, staging)
     ETS.foldl(fn({k,v}, a) -> f.({:c, k}, v, a) end, acc, commits)
  end

  def unpack(T[staging: staging, log: log, commits: commits], data) do
    tab =
    case data do
      {{:s, key}, value} -> staging
      {{:c, key}, value} -> commits
      {key, value} -> log
    end
    ETS.insert(tab, {key, value})
    :ok
  end

  def close(t) do
    delete(t)
    :ok
  end

end