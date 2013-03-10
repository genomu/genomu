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
    {:ok, State.new}
  end

end