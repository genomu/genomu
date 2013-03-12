defmodule Genomu.Watcher do

  use GenServer.Behaviour

  @spec start((term, term -> term), [Genomu.key]) :: {:ok, pid} | {:error, reason :: term}

  def start(fun, subscriptions) do
    :supervisor.start_child(Genomu.Sup.Watchers, [fun, subscriptions])
  end

  def start_link(fun, subscriptions) do
    :gen_server.start_link(__MODULE__, [fun, subscriptions], [])
  end

  defrecord State, subscriptions: [], fun: nil do
    record_type subscriptions: [Genomu.key], fun: ((term, term) -> term)
  end

  def init([fun, subscriptions]) do
    :gen_server.cast(self, :init)
    {:ok, State.new(fun: fun, subscriptions: subscriptions)}
  end

  def handle_cast(:init, State[subscriptions: subscriptions] = state) do
    lc subscription inlist subscriptions do
      :gproc_ps.subscribe(:l, {Genomu.Transaction, subscription})
    end
    {:noreply, state}
  end

  def handle_info({:gproc_ps_event, {Genomu.Transaction, subscription}, commit_object},
                  State[fun: fun] = state) do
    fun.(subscription, commit_object)
    {:noreply, state}
  end


end