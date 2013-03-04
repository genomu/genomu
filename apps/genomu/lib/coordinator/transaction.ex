defimpl Genomu.Coordinator.Protocol, for: Genomu.Transaction do

  defrecord State, entries: nil do
    record_type entries: Dict.t
  end

  def init(_txn) do
    {:ok, State.new}
  end

  def quorums(Genomu.Transaction[log: log, 
                                 n: n, r: r, vnodes: vnodes] = txn, State[] = state) do
    quorums = 
    Enum.reduce(log, HashDict.new,
                fn({key, clock}, dict) ->
                   preflist = get_preflist(key, txn)
                   ref = make_ref
                   quorum = Genomu.Coordinator.Quorum.new(ref: ref, 
                            n: n, r: r, vnodes: vnodes, preflist: preflist)
                   entry = {key, ITC.encode_binary(clock)}
                   Dict.update(dict, quorum, [entry], [entry|&1])
                end) |> Dict.to_list
    {:ok,
     (lc {quorum, _} inlist quorums, do: quorum),
     state.entries(lc {Genomu.Coordinator.Quorum[]=quorum, entries} inlist quorums, do: {quorum.ref, entries})}
  end

  # NB: once introduced, these values
  # should NOT change!
  defmacrop object_key_version, do: -1
  defmacrop object_key_n, do: 0
  defmacrop object_key_r, do: 1
  defmacrop object_key_vnodes, do: 2

  def message(Genomu.Transaction[] = txn, Genomu.Coordinator.Quorum[] = quorum, 
              State[entries: entries] = state) do
    entries = entries[quorum.ref]
    txn_object = ([
                   {object_key_version, 0},
                   {object_key_n, txn.n},
                   {object_key_r, txn.r},
                   {object_key_vnodes, (if txn.vnodes == :any, do: 0, else: 1)},
                 ] ++
                 entries) |> MsgPack.Map.from_list |> MsgPack.pack
    message = {:C, ITC.encode_binary(txn.clock), txn_object, entries, quorum.ref}
    {:ok, message, state}
  end

  def handle_response(Genomu.Transaction[], :ok, _partition, quorum, state) do
    {:ok, quorum, state}
  end
  def handle_response(Genomu.Transaction[], _other, _partition, quorum, state) do
    {:ok, quorum, state}
  end

  def finalize(Genomu.Transaction[], State[] = state) do
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
  def hash(key), do: :crypto.sha(key)


end