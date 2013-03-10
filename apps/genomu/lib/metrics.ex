defmodule Genomu.Metrics do

  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  defrecord State, []

  alias :folsom_metrics, as: M

  def init(_) do
    M.new_counter({__MODULE__, Connections})
    M.new_counter({__MODULE__, Channels})
    M.new_histogram({__MODULE__, ChannelResponseTime})
    M.new_histogram({__MODULE__, PartitionResponseTime})
    M.new_histogram({__MODULE__, QuorumTime})
    {:ok, State.new}
  end

end