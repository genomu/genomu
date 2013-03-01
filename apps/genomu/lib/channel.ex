defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  @spec start :: {:ok, pid} | {:error, reason :: term}
  def start do
    Genomu.Channel.fork Genomu.Channel.Root
  end

  @spec start_link(root :: atom | boolean, parent :: nil | pid | atom, interval :: ITC.t) :: {:ok, pid} | {:error, reason :: term}
  def start_link(false, parent, clock) do
    :gen_server.start_link(__MODULE__, {false, parent, clock}, [])
  end
  def start_link(true, parent, clock) do
    :gen_server.start_link(__MODULE__, {true, parent, clock}, [])
  end
  def start_link(name, parent, clock) do
    :gen_server.start_link({:local, name}, __MODULE__, {true, parent, clock}, [])
  end

  defrecord State, root: false, parent: nil,
                   clock: nil,
                   snapshot: nil, log: [],
                   children: nil do

    record_type root: boolean, parent: nil | pid | atom,
                clock: nil | ITC.t,
                snapshot: nil | :ets.tid, log: [Genomu.Transaction.entry],
                children: nil | :ets.tid

    @spec memoize(Genomu.key, t) :: t
    def memoize(key, __MODULE__[snapshot: snapshot, clock: clock, log: log] = state) do
      :ets.insert(snapshot, {key, clock})
      state.log([{key, clock}|log])
    end

    @spec lookup(Genomu.key, t) :: nil | ITC.t
    def lookup(key, __MODULE__[snapshot: snapshot]) do
      case :ets.lookup(snapshot, key) do
        [] -> nil
        [{^key, clock}] -> ITC.encode_binary(clock)
      end
    end

  end

  def init({root, parent, clock}) do
    {:ok, State.new(root: root, parent: parent, clock: clock)}
  end

  @spec clock(pid | atom) :: ITC.t
  defcall clock, state: State[clock: clock] = state do
    {:reply, clock, state}
  end

  @spec root?(pid | atom) :: boolean
  defcall root?, state: State[root: root] = state do
    {:reply, root, state}
  end

  @spec fork(pid | atom) :: ITC.t
  @spec fork(pid | atom, root :: atom | boolean) :: ITC.t
  def fork(server), do: fork(server, false)

  defcall fork(root), state: State[clock: clock] = state do
    state = ensure_children(state)

    {new_clock, channel_clock} = ITC.fork(clock)
    {:ok, channel} = :supervisor.start_child(Genomu.Sup.Channels, [root, self, channel_clock])
    Process.monitor(channel)
    :ets.insert(state.children,
                [{channel, channel_clock},
                 {channel_clock, channel}])
    state = state.clock(new_clock)
    {:reply, {:ok, channel}, state}
  end

  @spec sync(pid | atom, ITC.t) :: ITC.t
  defcall sync(clock0), state: State[clock: clock] = state do
    new_clock = ITC.join(clock, clock0)
    {clock1, clock2} = ITC.fork(new_clock)
    {:reply, clock2, state.clock(clock1)}
  end

  defcast update(channel, new_clock), state: State[children: c] = state do
    case :ets.lookup(c, channel) do
      [{^channel, clock}] ->
        :ets.insert(c, [{channel, new_clock}, {new_clock, channel}])
        :ets.delete(c, clock)
      [] -> 
        :ok # TODO: log a warning?
    end
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, _},
                  __MODULE__.State[clock: clock, children: c] = state) do
    [{^pid, child_clock}] = :ets.lookup(c, pid)
    :ets.delete(c, pid)
    :ets.delete(c, child_clock)
    new_clock = ITC.join(clock, child_clock)
    state = state.clock(new_clock)
    {:noreply, state}
  end  

  @spec execute(pid, Genomu.key, Genomu.command, Keyword.t) :: term
  defcall execute(key, cmd, options), state: State[] = state do
    state = ensure_snapshot(state)

    clock = next_clock(state.clock, cmd)
    cell = {key, state.lookup(key)}
    revision = ITC.encode_binary(clock)
    cmd = cmd.update(cell: cell, new_revision: revision)
    coord_options = [for: cmd] |>
                    Keyword.merge(options)
    result = Genomu.Coordinator.run(coord_options)
    state = memoize(key, cmd, clock, state)
    {:reply, result, state}
  end

  @spec commit(pid | atom) :: :ok | {:error, reason :: term}
  defcall commit, state: State[parent: parent, clock: clock, log: log] = state do
    clock = sync(parent, clock)
    txn = Genomu.Transaction.new(clock: clock, log: Enum.reverse(log))
    Genomu.Coordinator.run(for: txn)
    update(parent, self, clock)
    {:reply, :ok, state.clock(clock)}
  end

  @spec stop(pid | atom) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
  end

  @spec next_clock(ITC.t, Genomu.command) :: ITC.t
  defp next_clock(clock, Genomu.Command[type: :get]), do: clock
  defp next_clock(clock, _), do: ITC.event(clock)

  @spec memoize(Genomu.key, Genomu.command, ITC.t, State.t) :: State.t
  defp memoize(_key, Genomu.Command[type: :get], _clock, state) do
    state
  end
  defp memoize(key, _cmd, clock, State[] = state) do
    state.clock(clock).memoize(key)
  end

  defp ensure_snapshot(State[snapshot: nil] = state) do
    state.snapshot(:ets.new(__MODULE__.Snapshot, [:ordered_set]))
  end
  defp ensure_snapshot(State[] = state) do
    state
  end

  defp ensure_children(State[children: nil] = state) do
    state.children(:ets.new(__MODULE__.Children, [:ordered_set]))
  end
  defp ensure_children(State[] = state) do
    state
  end


end