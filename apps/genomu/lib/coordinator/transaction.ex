defimpl Genomu.Coordinator.Protocol, for: Genomu.Transaction do

  require Genomu.Constants.CommitObject
  alias Genomu.Constants.CommitObject, as: CO
  
  defrecord State, entries: nil do
    record_type entries: Dict.t
  end

  def init(_txn) do
    {:ok, State.new}
  end

  def quorums(Genomu.Transaction[log: log, 
                                 n: n, r: r, vnodes: vnodes] = txn, State[] = state) do
    {top, _} = Enum.reduce(log, {[], []},
                           fn({k, r}, {acc, keys}) ->
                             if List.member?(keys, k) do
                               {acc, keys}
                             else
                               {[{k, r}|acc], [k|keys]}
                             end
                           end)
    quorums =
    Enum.reduce(top, HashDict.new,
                fn({key, clock}, dict) ->
                   preflist = get_preflist(key, txn)
                   ref = make_ref
                   quorum = Genomu.Coordinator.Quorum.new(ref: ref, 
                            n: n, r: r, vnodes: vnodes, preflist: preflist)
                   entry = {key, clock}
                   Dict.update(dict, quorum, [entry], [entry|&1])
                end) |> Dict.to_list
    {:ok,
     (lc {quorum, _} inlist quorums, do: quorum),
     state.entries(lc {Genomu.Coordinator.Quorum[]=quorum, entries} inlist quorums, do: {quorum.ref, entries})}
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
  defp hash(key), do: :crypto.sha(key)


end