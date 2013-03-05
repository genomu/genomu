defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  @spec start :: {:ok, pid} | {:error, reason :: term}
  def start do
    Genomu.Channel.fork Genomu.Channel.Root
  end

  @spec start_link(root :: atom | boolean, 
                   parent :: nil | Genomu.gen_server_ref, 
                   clock :: ITC.t | nil) :: {:ok, pid} | {:error, reason :: term}
  def start_link(false, parent, clock) do
    :gen_server.start_link(__MODULE__, {false, parent, clock}, [])
  end
  def start_link(true, parent, clock) do
    :gen_server.start_link(__MODULE__, {true, parent, clock}, [])
  end
  def start_link(name, parent, clock) do
    :gen_server.start_link({:local, name}, __MODULE__, {name, parent, clock}, [])
  end

  defrecord State, root: false, parent: nil,
                   clock: nil, crash_clock: nil,
                   outstanding: nil,
                   snapshot: nil, log: [],
                   children: nil do

    record_type root: atom | boolean, parent: nil | Genomu.gen_server_ref,
                clock: nil | ITC.t, crash_clock: nil | ITC.t,
                outstanding: nil | ITC.t,
                snapshot: nil | :ets.tid, log: [Genomu.Transaction.entry],
                children: nil | :ets.tid

    @spec initialize(t) :: t
    def initialize(__MODULE__[root: false] = state) do
      state
    end
    # If it's a root channel, and the clock is not
    # specified, read it
    def initialize(__MODULE__[clock: nil] = state) do
      crash_clock = read_crash_clock(state)
      state.crash_clock(crash_clock).clock(crash_clock)
    end
    def initialize(__MODULE__[] = state) do
      state
    end

    @spec join_outstanding(ITC.t, t) :: t
    def join_outstanding(new, __MODULE__[outstanding: nil] = state) do
      state.outstanding(new)
    end
    def join_outstanding(new, __MODULE__[outstanding: clock] = state) do
      try do
        state.outstanding(ITC.join(clock, new))
      catch _, _ -> # if we can't join, then we already did                    
        state
      end
    end

    defoverridable crash_clock: 2
    def crash_clock(new_crash_clock, state) do
      dump_crash_clock(super(new_crash_clock, state))
    end

    @spec dump_crash_clock(t) :: t
    defp dump_crash_clock(__MODULE__[crash_clock: crash_clock] = state)  do
      File.write(crash_clock_filename(state), ITC.encode_binary(crash_clock))
      state
    end

    @spec read_crash_clock(t) :: ITC.t
    defp read_crash_clock(__MODULE__[] = state) do
      if File.exists?(crash_clock_filename(state)) do
        File.read!(crash_clock_filename(state)) |> ITC.decode
      else
        ITC.seed
      end
    end

    @spec crash_clock_filename(t) :: String.t
    defp crash_clock_filename(__MODULE__[root: root]) do
      data_dir = Application.environment(:genomu)[:data_dir]
      Path.join(data_dir, "#{inspect root}")
    end

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
    :folsom_metrics.new_counter(Genomu.Metrics.Channels)
    :folsom_metrics.notify({Genomu.Metrics.Channels, {:inc, 1}})
    :erlang.process_flag(:trap_exit, true)
    {:ok, State.new(root: root, parent: parent, clock: clock).initialize}
  end

  @spec clock(Genomu.gen_server_ref) :: ITC.t
  defcall clock, state: State[clock: clock] = state do
    {:reply, clock, state}
  end

  @spec root?(Genomu.gen_server_ref) :: boolean
  defcall root?, state: State[root: root] = state do
    {:reply, root != false, state}
  end

  @spec fork(Genomu.gen_server_ref) :: {:ok, pid}
  @spec fork(Genomu.gen_server_ref, root :: atom | boolean) :: {:ok, pid}
  def fork(server), do: fork(server, false)

  defcall fork(root), state: State[clock: clock] = state do
    state = ensure_children(state)

    {new_clock, channel_clock} = ITC.fork(clock)
    {:ok, channel} = :supervisor.start_child(Genomu.Sup.Channels, [root, self, channel_clock])
    Process.monitor(channel)
    :ets.insert(state.children,
                [{channel, channel_clock},
                 {channel_clock, channel}])
    state = state.clock(new_clock).join_outstanding(channel_clock)
    {:reply, {:ok, channel}, state}
  end

  @spec fork_root(Genomu.gen_server_ref) :: ITC.t
  defcall fork_root, state: State[clock: clock, crash_clock: crash_clock] = state do
    {new_clock, fork_clock} = ITC.fork(clock)
    {new_crash_clock, _} = ITC.fork(crash_clock)
    state = state.clock(new_clock).crash_clock(new_crash_clock)
    {:reply, fork_clock, state}
  end

  @spec sync(Genomu.gen_server_ref, ITC.t) :: ITC.t
  defcall sync(clock0), state: State[clock: clock] = state do
    new_clock = ITC.join(clock, clock0)
    {clock1, clock2} = ITC.fork(new_clock)
    {:reply, clock2, state.clock(clock1)}
  end

  @spec sync_with(server :: Genomu.gen_server_ref, sync_with :: Genomu.gen_server_ref) :: :ok
  defcall sync_with(sync_with), state: State[] = state do
    clock = fork_root(sync_with)
    {:reply, :ok, state.clock(clock).parent(sync_with).crash_clock(clock)}
  end


  defcast update(channel, new_clock), state: State[children: c] = state do
    case :ets.lookup(c, channel) do
      [{^channel, clock}] ->
        :ets.insert(c, [{channel, new_clock}, {new_clock, channel}])
        :ets.delete(c, clock)
      [] -> 
        :ok # TODO: log a warning?
    end
    {:noreply, state.join_outstanding(new_clock)}
  end

  def handle_info({:DOWN, _ref, :process, pid, _},
                  __MODULE__.State[clock: clock, children: c] = state) do
    [{^pid, child_clock}] = :ets.lookup(c, pid)
    :ets.delete(c, pid)
    :ets.delete(c, child_clock)
    new_clock = ITC.join(clock, child_clock)
    crash_clock = ITC.join(clock, state.outstanding)
    state = state.clock(new_clock).crash_clock(crash_clock)
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

  @spec commit(Genomu.gen_server_ref) :: :ok | {:error, reason :: term}
  defcall commit, state: State[parent: parent, clock: clock, log: log] = state do
    clock = sync(parent, clock)
    txn = Genomu.Transaction.new(clock: clock, log: Enum.reverse(log))
    Genomu.Coordinator.run(for: txn)
    update(parent, self, clock)
    {:reply, :ok, state.clock(clock)}
  end

  @spec stop(Genomu.gen_server_ref) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
  end

  def terminate(_, State[]) do
    :folsom_metrics.notify({Genomu.Metrics.Channels, {:dec, 1}})
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