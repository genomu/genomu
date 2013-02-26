defimpl Genomu.Coordinator.Protocol, for: Genomu.Command do

  defrecord State, acc: [] do
    record_type acc: [term]
  end

  def init(_command) do
    {:ok, State.new}
  end

  def quorums(Genomu.Command[n: n, r: r, vnodes: vnodes] = command, State[] = state) do
    preflist = get_preflist(command)
    {:ok,
     [Genomu.Coordinator.Quorum.new(ref: make_ref, preflist: preflist, n: n, r: r, vnodes: vnodes)],
     state}
  end

  def message(Genomu.Command[] = command, Genomu.Coordinator.Quorum[] = quorum, 
              State[] = state) do
    message = {command.cell, command.new_revision, 
               {command.type, command.operation}, quorum.ref}
    {:ok, message, state}
  end

  def handle_response(Genomu.Command[], response, partition, quorum, state) do
    state = state.update_acc([{partition, response}|&1])
    {:ok, quorum, state}
  end

  def finalize(Genomu.Command[], State[acc: acc] = state) do
    [first_value|_] = values =
        lc {_partition, value} inlist acc, do: value
    if Enum.any?(values, fn(v) -> v != first_value end) do
      {:reply, {:conflict, values}, state}
    else
      {:reply, first_value, state}
    end
  end

  @spec get_preflist(Genomu.Command.t) :: :riak_core_apl.preflist
  defp get_preflist(Genomu.Command[cell: {key, _rev}, vnodes: :primary] = command) do
    lc {{partition, node}, _type} inlist
       :riak_core_apl.get_primary_apl(hash(key), command.n, Genomu.VNode) do
      {partition, node}
    end
  end

  defp get_preflist(Genomu.Command[cell: {key, _rev}] = command) do
    :riak_core_apl.get_apl(hash(key), command.n, Genomu.VNode)
  end

  @spec hash(Genomu.key) :: Genomu.Coordinator.index
  def hash(key), do: :crypto.sha(key)

end