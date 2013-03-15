defrecord Genomu.Storage.Bitcask, path: nil, ref: nil, 
                                  opts: [read_write: true]

defimpl Genomu.Storage, for: Genomu.Storage.Bitcask do

  alias :bitcask, as: B
  alias Genomu.Storage.Bitcask, as: T


  @object_page_size 32

  @nil_value MsgPack.pack(nil)
  @log_prefix <<0>>
  @commit_prefix <<1>>
  @zero_value MsgPack.pack(0)

  def init(T[] = t, options) do
     File.mkdir_p(t.path)
     ref = B.open(to_char_list(Path.join(t.path, "#{options[:partition]}")), Keyword.put(t.opts, :read_write, t.opts[:read_write] || true))
     {:ok, t.ref(ref)}
  end

  def lookup(T[ref: ref], {key, nil}) do
     case B.get(ref, @log_prefix <> Enum.join(key)) do
       :not_found -> page_bin = @zero_value
       {:ok, page_bin} -> :ok
     end
     case B.get(ref, @log_prefix <> Enum.join(key) <> page_bin) do
       {:ok, binary} ->
         {_page_ctr, binary} = MsgPack.unpack(binary)
         {value, binary} = MsgPack.next(binary)
         {op, binary} = MsgPack.unpack(binary)
         {clock, _rest} = MsgPack.unpack(binary)
         case clock do
           [vsn, txn] -> :ok
           vsn when is_binary(vsn) -> txn = vsn
         end
         {value, op, vsn, txn}
       :not_found -> {@nil_value, "", "", ""}
     end
  end

  def lookup(T[ref: ref] = t, {key, rev} = cell) do
    case B.get(ref, Enum.join(key) <> rev) do
      {:ok, binary} ->
        {value, op} = MsgPack.next(binary)
        {value, op, rev, rev}
      :not_found -> lookup_cell_txn(t, cell)
    end
  end

  defp lookup_cell_txn(T[ref: ref] = t, {key, _} = cell) do
    case B.get(ref, @log_prefix <> Enum.join(key)) do
      :not_found -> page = 0
      {:ok, bin} -> {page, ""} = MsgPack.unpack(bin)
    end
    lookup_cell_txn(t, cell, page)
  end
  defp lookup_cell_txn(T[ref: ref] = t, {key, rev} = cell, page) do
    case B.get(ref, @log_prefix <> Enum.join(key) <> MsgPack.pack(page)) do
      :not_found ->
        {@nil_value, "", "", ""}
      {:ok, binary} ->
        {_page_ctr, binary} = MsgPack.next(binary)
        {value, binary} = MsgPack.next(binary)
        {op, binary} = MsgPack.unpack(binary)
        {clock, history} = MsgPack.unpack(binary)
        case clock do 
          [entry_clock, ^rev] ->
            {value, op, entry_clock, rev}
          ^rev ->
            {value, op, rev, rev}
          _ ->
            case iterate_history(history, rev) do
              {entry_clock, txn_rev} ->
                {:ok, binary} = B.get(ref, Enum.join(key) <> entry_clock)
                {value, _binary} = MsgPack.next(binary)
                {op, _binary} = MsgPack.unpack(binary)
                {value, op, entry_clock, txn_rev}
              nil ->
                if page == 0 do
                  {@nil_value, "", "", ""}
                else
                  lookup_cell_txn(t, cell, page - 1)
                end
              end
        end
    end
  end

  defp iterate_history("", _rev), do: nil
  defp iterate_history(binary, rev) do
    {clock, rest} = MsgPack.unpack(binary)
    case clock do
      [entry_clock, txn_clock] -> :ok
      entry_clock -> txn_clock = entry_clock
    end
    if entry_clock == rev or
       ITC.decode(entry_clock) |>
       ITC.le(ITC.decode(rev)) do
      {entry_clock, txn_clock}
    else
     iterate_history(rest, rev)
    end
  end

  def commit(T[ref: ref], revision, entries, txn_log, commit_object) do
    insertions =
    Enum.reduce(entries, [], fn({key, entry_clock}, i) ->
      case B.get(ref, @log_prefix <> Enum.join(key)) do
        :not_found -> 
          page = 0
          case B.get(ref, @log_prefix <> Enum.join(key) <> MsgPack.pack(page)) do
            :not_found -> history = <<>>; value = @nil_value ; page_ctr = 0
            {:ok, binary} ->
              {page_ctr, binary} = MsgPack.unpack(binary)
              {value, binary} = MsgPack.next(binary)
              {operation, history} = MsgPack.unpack(binary)
          end
        {:ok, page_bin} ->
          {page, _} = MsgPack.unpack(page_bin)
          {:ok, binary} = B.get(ref, @log_prefix <> Enum.join(key) <> MsgPack.pack(page))
          {page_ctr, binary} = MsgPack.unpack(binary)
          {value, binary} = MsgPack.next(binary)
          {operation, history} = MsgPack.unpack(binary)
      end
      if history == <<>> do
        prev_version = ""
      else 
        case MsgPack.unpack(history) do
          {[prev_version, _], _} -> :ok
          {prev_version, _} -> :ok
        end
      end
      {value, operation, updates} = recalculate(txn_log, prev_version, key, operation, value, [], ref)
      history_entry = history_entry(entry_clock, revision)
      if page_ctr <= @object_page_size do
        [{@log_prefix <> Enum.join(key) <> MsgPack.pack(page),
          MsgPack.pack(page_ctr + 1) <>
          value <> MsgPack.pack(operation) <> history_entry <> history}|updates] ++ i
      else
        # allocate a new page
         [{@log_prefix <> Enum.join(key) <> MsgPack.pack(page + 1), MsgPack.pack(1) <>
                   value <> MsgPack.pack(operation) <> history_entry},
          {@log_prefix <> Enum.join(key), MsgPack.pack(page + 1)}|updates] ++ i
      end
    end)
    lc {ik, iv} inlist insertions do
        B.put(ref, ik, iv)
    end
    B.put(ref, @commit_prefix <> revision, commit_object)
  end


  defp recalculate([], _, _, operation, acc, updates, _ref), do: {acc, operation, updates}
  defp recalculate([{k, ver}|t], prev_version, k, operation, acc, updates, ref) do
    {:ok, binary} = B.get(ref, Enum.join(k) <> ver)
    {_value, op} = MsgPack.next(binary)
    case Genomu.VNode.apply_operation(op, acc, [operation: operation, version: prev_version]) do
      {:error, exception} ->
        raise Genomu.VNode.AbortCommitException, exception: exception
      Genomu.Operation.AbortException[] = e ->
        raise Genomu.VNode.AbortCommitException, exception: e
      acc ->
        recalculate(t, ver, k, op, acc, [{Enum.join(k) <> ver, acc <> op}|updates], ref)
    end
  end
  defp recalculate([_|t], prev_version, k, operation, acc, updates, ref) do
    recalculate(t, prev_version, k, operation, acc, updates, ref)
  end

  @compile {:inline, [history_entry: 2]}
  defp history_entry(clock, clock), do: MsgPack.pack(clock)
  defp history_entry(entry_clock, clock) do
    MsgPack.pack([entry_clock, clock])
  end

  def stage(T[ref: ref], {key, rev}, operation, value) do
     B.put(ref, Enum.join(key) <> rev, value <> operation)
  end


  def delete(T[ref: ref]) do
    B.fold(ref, fn(k, _, _) -> B.delete(ref, k) end, nil) # TODO: better wipe out
  end

  def size(T[ref: ref]) do
    B.fold(ref, fn(_, _, c) -> c + 1 end, 0)
  end

  def reduce(T[ref: ref], acc0, f) do
    B.fold(ref, f, acc0)
  end

  def unpack(T[ref: ref], {key, value}) do
    B.put(ref, key, value)
  end

  def close(T[ref: ref]) do
    B.close(ref)
    :ok
  end

end