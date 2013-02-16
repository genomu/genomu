defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  @spec start_link(ITC.t) :: {:ok, pid} | {:error, reason :: term}
  def start_link(interval) do
    :gen_server.start_link(__MODULE__, interval, [])
  end

  @spec start_link :: {:ok, pid} | {:error, reason :: term}
  def start_link do
    start_link(Genomu.Interval.fork)
  end

  @spec start :: {:ok, pid} | {:error, reason :: term}
  def start do
    :supervisor.start_child(Genomu.Sup.Channels, [])
  end

  defrecord State, interval: nil,
                   snapshot: nil do

    record_type interval: nil | ITC.t,
                snapshot: :ets.tid

    def blank(opts) do
      snapshot = :ets.new(__MODULE__.Snapshot, [:ordered_set])
      new(Keyword.merge([snapshot: snapshot], opts))
    end

    @spec memoize(Genomu.key, t) :: t
    def memoize(key, __MODULE__[snapshot: snapshot, interval: i] = state) do
      :ets.insert(snapshot, {key, i})
      state
    end

    @spec lookup(Genomu.key, t) :: nil | ITC.t
    def lookup(key, __MODULE__[snapshot: snapshot]) do
      case :ets.lookup(snapshot, key) do
        [] -> nil
        [{^key, i}] -> i
      end
    end

  end

  use Genomu.Interval.Server

  def init(interval) do
    {:ok, State.blank(interval: interval)}
  end

  @spec execute(pid, Genomu.key, Genomu.command, Keyword.t) :: term
  defcall execute(key, cmd, options), state: State[] = state do
    interval = next_interval(state.interval, cmd)
    cell = {key, state.lookup(key)}
    coordination_options = Keyword.merge([cell: cell,
                                          command: cmd,
                                          interval: interval], options)
    result = Genomu.Coordinator.run(coordination_options)
    state = memoize(key, cmd, interval, state)
    {:reply, result, state}
  end


  @spec next_interval(ITC.t, Genomu.command) :: ITC.t
  defp next_interval(i, {:get, _}), do: i
  defp next_interval(i, _), do: ITC.event(i)

  @spec memoize(Genomu.key, Genomu.command, ITC.t, State.t) :: State.t
  defp memoize(_key, {:get, _}, _interval, state) do
    state
  end
  defp memoize(key, _cmd, interval, State[] = state) do
    state.interval(interval).memoize(key)
  end

end