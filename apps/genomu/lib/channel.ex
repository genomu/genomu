defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  require Lager

  @spec start :: {:ok, pid}
  def start do
    Genomu.Channel.Root.fork
  end

  @spec start_link(clock :: ITC.t | nil) :: {:ok, pid} | {:error, reason :: term}
  def start_link(clock) do
    :gen_server.start_link(__MODULE__, clock, [])
  end

  defrecord State, initial_clock: nil,
                   clock: nil,
                   snapshot: nil, log: [],
                   modified: false, joined: false do

    record_type initial_clock: nil | ITC.t, clock: nil | ITC.t,
                snapshot: nil | :ets.tid, log: [Genomu.Transaction.entry],
                modified: boolean, joined: boolean

    @spec memoize(Genomu.key, binary, t) :: t
    def memoize(key, rev, __MODULE__[snapshot: snapshot, log: log] = state) do
      :ets.insert(snapshot, {key, rev})
      state.log([{key, rev}|log])
    end

    @spec lookup(Genomu.key, t) :: nil | ITC.t
    def lookup(key, __MODULE__[snapshot: snapshot]) do
      case :ets.lookup(snapshot, key) do
        [] -> nil
        [{^key, rev}] -> rev
      end
    end

  end

  def init(clock) do
    :folsom_metrics.notify({{Genomu.Metrics, Channels}, {:inc, 1}})
    :erlang.process_flag(:trap_exit, true)
    {:ok, State[clock: clock, initial_clock: clock]}
  end

  @spec execute(pid, Genomu.key | Genomu.cell, Genomu.command, Keyword.t, term, pid) :: :ok
  defcast execute(key, Genomu.Command[] = cmd, options, tag, pid), state: State[] = state do
    assertion = cmd.assertion?
    if assertion and state.modified do
      pid <- {tag, :abort}
      {:noreply, state}
    else
      State[] = state = ensure_snapshot(state)
      clock = next_clock(state.clock, cmd)
      cell = get_cell(key, state)
      revision = ITC.encode_binary(clock)
      Genomu.Command[] = cmd = cmd.cell(cell).new_revision(revision)
      coord_options = [for: cmd] |>
                      Keyword.merge(options)
      result = Genomu.Coordinator.run(coord_options)
      State[] = state = memoize(key, cmd, clock, revision, state)
      pid <- {tag, result}
      {:noreply, state.modified((not assertion) and cmd.type in [:set, :apply])}
    end
  end

  @spec commit(Genomu.gen_server_ref, Genomu.Transaction.t, term, pid) :: :ok
  defcast commit(Genomu.Transaction[] = txn, tag, pid), 
          state: State[initial_clock: initial_clock, clock: clock, log: log] = state do
    case log do
      [] ->
        pid <- {tag, :ok}
        Genomu.Channel.Root.join(initial_clock, clock)
        {:stop, :normal, state.joined(true)}
      _ ->
        txn = txn.clock(clock).log(Enum.reverse(log))
        result = Genomu.Coordinator.run(for: txn)
        pid <- {tag, result}
        Genomu.Channel.Root.join(initial_clock, clock)
        {:stop, :normal, state.clock(clock).joined(true)}
    end
  end

  @spec discard(Genomu.gen_server_ref, term, pid) :: :ok
  defcast discard(tag, pid), state: State[initial_clock: initial_clock, clock: clock] = state do
    stop(self)
    Genomu.Channel.Root.join(initial_clock, clock)
    pid <- {tag, :ok}
    {:noreply, state.joined(true)}
  end

  @spec stop(Genomu.gen_server_ref) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
  end

  def terminate(_, State[joined: true]) do
    :folsom_metrics.notify({{Genomu.Metrics, Channels}, {:dec, 1}})
  end

  def terminate(reason, State[initial_clock: initial_clock, clock: clock] = state) do
    Genomu.Channel.Root.join(initial_clock, clock)
    terminate(reason, state.joined(true))
  end

  @spec next_clock(ITC.t, Genomu.command) :: ITC.t
  defp next_clock(clock, Genomu.Command[type: :get]), do: clock
  defp next_clock(clock, Genomu.Command[type: :operation]), do: clock
  defp next_clock(clock, _), do: ITC.event(clock)

  @spec memoize(Genomu.key, Genomu.command, ITC.t, binary, State.t) :: State.t
  defp memoize(_key, Genomu.Command[type: :get], _clock, _rev, state) do
    state
  end
  defp memoize(_key, Genomu.Command[type: :operation], _clock, _rev, state) do
    state
  end
  defp memoize(key, _cmd, clock, rev, State[] = state) do
    state.clock(clock).memoize(key, rev)
  end

  defp ensure_snapshot(State[snapshot: nil] = state) do
    state.snapshot(:ets.new(__MODULE__.Snapshot, [:ordered_set]))
  end
  defp ensure_snapshot(State[] = state) do
    state
  end

  defp get_cell(key, state) when is_list(key) do
    {key, state.lookup(key)}
  end
  defp get_cell({_, _} = cell, _state), do: cell


end