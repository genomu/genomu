defimpl Genomu.Coordinator.Protocol, for: Genomu.Transaction do

  require Genomu.Constants.CommitObject
  alias Genomu.Constants.CommitObject, as: CO
  
  defrecord State, entries: nil, commit_object: nil, keys: [] do
    record_type entries: Dict.t, commit_object: term, keys: [Genomu.key]
  end

  def init(_txn) do
    {:ok, State.new}
  end

  @compile {:inline, get_quorums: 5, get_preflist: 2}
  defp get_quorums([], _qt, _txn, entries, quorums), do: {quorums, entries}
  defp get_quorums([{key, _} = cell|t], Genomu.Coordinator.Quorum[] = qt, txn, entries, quorums) do
      preflist = get_preflist(key, txn)
      ref = :erlang.phash2(preflist)
      case List.keyfind(entries, ref, 0) do
        {_, cells} ->
          cells = List.keystore(cells, key, 0, cell)
          entries = List.keyreplace(entries, ref, 0, {ref, cells})
        _ ->
          quorum = qt.update(ref: ref, preflist: preflist)
          entries = [{ref, [cell]}|entries]
          quorums = [quorum|quorums]
      end
      get_quorums(t, qt, txn, entries, quorums)
  end

  def quorums(Genomu.Transaction[log: log, 
                                 n: n, r: r, vnodes: vnodes] = txn, State[] = state) do
    quorum_template = Genomu.Coordinator.Quorum.new(n: n, r: r, vnodes: vnodes)
    Genomu.Coordinator.Quorum[] = quorum_template
    {quorums, entries} = get_quorums(log, quorum_template, txn, [], [])
    keys = Enum.uniq(Keyword.keys(log))
    {:ok, quorums, state.entries(entries).keys(keys)}
  end

  def message(Genomu.Transaction[log: log] = txn, Genomu.Coordinator.Quorum[] = quorum, 
              State[entries: entries] = state) do
    entries = entries[quorum.ref]
    txn_object = [
                   {CO.version, 0},
                   {CO.n, txn.n},
                   {CO.r, txn.r},
                   {CO.vnodes, (if txn.vnodes == :any, do: 0, else: 1)},
                   {CO.timestamp, Genomu.Utils.now_in_microseconds},
                   {CO.host, Genomu.Utils.host_id},
                   {CO.log, MsgPack.Map.from_list(log)},
                 ] |> MsgPack.Map.from_list
    message = {:C, ITC.encode_binary(txn.clock), txn_object, entries, quorum.ref}
    {:ok, message, state.commit_object(txn_object)}
  end

  def handle_response(Genomu.Transaction[], :ok, _partition, quorum, state) do
    {:ok, quorum, state}
  end
  def handle_response(Genomu.Transaction[], _other, _partition, quorum, state) do
    {:ok, quorum, state}
  end

  def finalize(Genomu.Transaction[], State[keys: keys, commit_object: commit_object] = state) do
    spawn(fn ->
            lc key inlist keys do
              :gproc_ps.publish(:l, {Genomu.Transaction, key}, commit_object)
            end
          end)
    {:reply, :ok, state}
  end


  @spec get_preflist(Genomu.key, Genomu.Transaction.t) :: :riak_core_apl.preflist
  defp get_preflist(key, Genomu.Transaction[vnodes: :primary] = txn) do
    lc {{partition, node}, _type} inlist
       :riak_core_apl.get_primary_apl(hash(key), txn.n, :genomu) do
      {partition, node}
    end
  end

  defp get_preflist(key, Genomu.Transaction[] = txn) do
    :riak_core_apl.get_apl(hash(key), txn.n, :genomu)
  end

  @spec hash(Genomu.key) :: Genomu.Coordinator.index
  defp hash(key), do: :crypto.sha(key)


end