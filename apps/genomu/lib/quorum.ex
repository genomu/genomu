defmodule Genomu.Quorum do

  use GenServer.Behaviour

  def start_link(partition) do
    :gen_server.start_link({:local, :"#{__MODULE__}_#{partition}"}, __MODULE__, [partition], [])
  end

  def command(preflist, message, receiver) do
    [{quorum_start, node}|_] = preflist
    :gen_server.cast({:"#{__MODULE__}_#{quorum_start}", node}, {:command, preflist, message, receiver})
  end

  def init([partition]) do
    {:ok, partition}
  end

  def handle_cast({:command, preflist, message, receiver}, partition) do
    :riak_core_vnode_master.command(preflist, message, 
                                    receiver,
                                    Genomu.VNode_master)
    {:noreply, partition}
  end
  def handle_cast(_, partition) do
    {:noreply, partition}
  end


end