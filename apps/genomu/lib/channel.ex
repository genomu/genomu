defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  require Lager

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
                   clock: nil, crash_clock: nil, crash_clock_filename: nil,
                   outstanding: [],
                   snapshot: nil, log: [],
                   children: nil do

    record_type root: atom | boolean, parent: nil | Genomu.gen_server_ref,
                clock: nil | ITC.t, crash_clock: nil | ITC.t,
                crash_clock_filename: String.t,
                outstanding: [ITC.t],
                snapshot: nil | :ets.tid, log: [Genomu.Transaction.entry],
                children: nil | :ets.tid

    @spec initialize(t) :: t
    def initialize(__MODULE__[root: false] = state) do
      state
    end
    # If it's a root channel, and the clock is not
    # specified, read it
    def initialize(__MODULE__[root: root, clock: nil] = state) do
      data_dir = Application.environment(:genomu)[:data_dir]
      filename = Path.join(data_dir, "#{inspect root}")
      state = state.crash_clock_filename(filename)
      crash_clock = read_crash_clock(state)
      state.crash_clock(crash_clock).clock(crash_clock)
    end
    def initialize(__MODULE__[] = state) do
      state
    end

    defoverridable crash_clock: 2
    def crash_clock(new_crash_clock, state) do
      dump_crash_clock(super(new_crash_clock, state))
    end

    @spec dump_crash_clock(t) :: t
    defp dump_crash_clock(__MODULE__[crash_clock: crash_clock, crash_clock_filename: f] = state)  do
      File.write(f, ITC.encode_binary(crash_clock))
      state
    end

    @spec read_crash_clock(t) :: ITC.t
    defp read_crash_clock(__MODULE__[crash_clock_filename: f]) do
      if File.exists?(f) do
        File.read!(f) |> ITC.decode
      else
        ITC.seed
      end
    end

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

  def init({root, parent, clock}) do
    if root == false, do: :folsom_metrics.notify({{Genomu.Metrics, Channels}, {:inc, 1}})
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
    state = state.clock(new_clock).update_outstanding([channel_clock|&1])
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
        state = state.update_outstanding(fn(o) -> [new_clock|o] -- [clock] end)
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
    clock = ITC.join(clock, child_clock)
    state = state.clock(clock).update_outstanding(fn(o) -> o -- [child_clock] end)
    state = state.crash_clock(Enum.reduce(state.outstanding, clock, fn(c, c1) -> ITC.join(c, c1) end))
    {:noreply, state}
  end

  @spec execute(pid, Genomu.key | Genomu.cell, Genomu.command, Keyword.t) :: term
  defcall execute(key, cmd, options), state: State[] = state do
    state = ensure_snapshot(state)
    clock = next_clock(state.clock, cmd)
    cell = get_cell(key, state)
    revision = ITC.encode_binary(clock)
    cmd = cmd.update(cell: cell, new_revision: revision)
    coord_options = [for: cmd] |>
                    Keyword.merge(options)
    result = Genomu.Coordinator.run(coord_options)
    state = memoize(key, cmd, clock, revision, state)
    {:reply, result, state}
  end

  @spec commit(Genomu.gen_server_ref, Genomu.Transaction.t) :: :ok | {:error, reason :: term}
  defcall commit(Genomu.Transaction[] = txn), state: State[parent: parent, clock: clock, log: log] = state do
    case log do
      [] ->
        stop(self)
        {:reply, :ok, state}
      _ ->
        clock = sync(parent, clock)
        txn = txn.update(clock: clock, log: Enum.reverse(log))
        Genomu.Coordinator.run(for: txn)
        update(parent, self, clock)
        stop(self)
        {:reply, :ok, state.clock(clock)}
    end
  end

  @spec discard(Genomu.gen_server_ref) :: :ok
  defcall discard, state: state do
    stop(self)
    {:reply, :ok, state}
  end

  @spec stop(Genomu.gen_server_ref) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
  end

  def handle_info({:'EXIT', _pid, _reason}, state) do
    {:stop, :normal, state}
  end

  def terminate(_, State[root: false]) do
    :folsom_metrics.notify({{Genomu.Metrics, Channels}, {:dec, 1}})
  end
  def terminate(_, State[]) do
    :ok
  end

  @spec next_clock(ITC.t, Genomu.command) :: ITC.t
  defp next_clock(clock, Genomu.Command[type: :get]), do: clock
  defp next_clock(clock, _), do: ITC.event(clock)

  @spec memoize(Genomu.key, Genomu.command, ITC.t, binary, State.t) :: State.t
  defp memoize(_key, Genomu.Command[type: :get], _clock, _rev, state) do
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

  defp ensure_children(State[children: nil] = state) do
    state.children(:ets.new(__MODULE__.Children, [:ordered_set]))
  end
  defp ensure_children(State[] = state) do
    state
  end

  defp get_cell(key, state) when is_list(key) do
    {key, state.lookup(key)}
  end
  defp get_cell({_, _} = cell, _state), do: cell


end