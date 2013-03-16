defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  require Lager

  @nroot_channels :"mochiglobal:#{Genomu.Channel.NRootChannels}"
  @root_channels :"mochiglobal:#{Genomu.Channel.RootChannels}"

  @spec start :: {:ok, pid} | {:error, reason :: term}
  def start do
    i = :random.uniform(@nroot_channels.term)
    Genomu.Channel.fork elem(@root_channels.term, i - 1)
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
                   clock: nil, crash_clock: nil, crash_clock_file: nil,
                   outstanding: [],
                   snapshot: nil, log: [],
                   modified: false,
                   children: nil do

    record_type root: atom | boolean, parent: nil | Genomu.gen_server_ref,
                clock: nil | ITC.t, crash_clock: nil | ITC.t,
                crash_clock_file: term,
                outstanding: [ITC.t],
                snapshot: nil | :ets.tid, log: [Genomu.Transaction.entry],
                modified: true,
                children: nil | :ets.tid

    def crash_clock_filename(__MODULE__[root: root]) do
      data_dir = Application.environment(:genomu)[:data_dir]
      Path.join(data_dir, "#{inspect root}")
    end

    @spec initialize(t) :: t
    def initialize(__MODULE__[root: false] = state) do
      state
    end
    def initialize(__MODULE__[parent: parent] = state) do
      filename = crash_clock_filename(state)
      unless File.exists?(filename) do
        new_file = true
      end
      {:ok, file} = File.open(filename, [:binary, :raw, :read, :write])
      __MODULE__[] = state = state.crash_clock_file(file)
      if new_file do
          crash_clock =
            unless nil?(parent) do
              Genomu.Channel.fork(parent, self)
            else
              ITC.seed
            end
          Lager.debug "Channel #{inspect state.root}: assume clock #{inspect crash_clock}"
      else
          {:ok, bin} = :file.pread(file, 0, File.stat!(filename).size)
          crash_clock = ITC.decode(bin)
          Lager.debug "Channel #{inspect state.root}: recover clock #{inspect crash_clock}"
      end
      state.crash_clock(crash_clock).clock(crash_clock)
    end

    defoverridable crash_clock: 2
    def crash_clock(new_crash_clock, state) do
      dump_crash_clock(super(new_crash_clock, state))
    end

    @spec dump_crash_clock(t) :: t
    defp dump_crash_clock(__MODULE__[crash_clock: crash_clock, crash_clock_file: f] = state)  do
      :file.pwrite(f, 0, ITC.encode_binary(crash_clock))
      :file.datasync(f)
      state
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
    {:ok, State[root: root, parent: parent, clock: clock].initialize}
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
  @spec fork(Genomu.gen_server_ref, channel :: Genomu.gen_server_ref) :: ITC.t
  def fork(server), do: fork(server, nil)

  defcall fork(receiver), from: from, state: State[clock: clock, outstanding: o] = state do
    Lager.debug "Channel fork #{inspect receiver} state: #{inspect state}"
    State[] = state = ensure_children(state)
    {new_clock, channel_clock} = ITC.fork(clock)
    if nil?(receiver) do
      {:ok, channel} = :supervisor.start_child(Genomu.Sup.Channels, [false, self, channel_clock])
      :gen_server.reply(from, {:ok, channel})
    else
      :gen_server.reply(from, channel_clock)
      channel = receiver
    end
    ref = Process.monitor(channel)
    :ets.insert(state.children,
                [{{:ref, ref}, channel},
                 {channel, channel_clock},
                 {channel_clock, channel}])
    state = state.clock(new_clock).outstanding([channel_clock|o])
    {:noreply, state}
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
  defcall sync_with(sync_with), state: State[root: Genomu.Channel.Root, children: c] = state do
    clock = fork_root(sync_with)
    Lager.debug "Channel #{inspect Genomu.Channel.Root}: sync_with #{inspect sync_with}, assume clock #{inspect clock}"
    :ets.match(c, {{:ref, :"$1"}, :_}) |> Enum.map(fn([ref]) -> Process.demonitor(ref) end)
    :ets.delete_all_objects(c)
    lc ch inlist tuple_to_list(:mochiglobal.get(Genomu.Channel.RootChannels)), do: wipeout(ch)
    :ok = :supervisor.terminate_child(:genomu_sup, Genomu.Sup.Channels)
    {:ok, _child} = :supervisor.restart_child(:genomu_sup, Genomu.Sup.Channels)
    state = state.clock(clock).parent(sync_with).crash_clock(clock).outstanding([])
    {:reply, :ok, state}
  end


  defcast update(channel, new_clock), state: State[children: c, outstanding: o] = state do
    case :ets.lookup(c, channel) do
      [{^channel, clock}] ->
        :ets.insert(c, [{channel, new_clock}, {new_clock, channel}])
        :ets.delete(c, clock)
        state = state.outstanding([new_clock|o] -- [clock])
      [] -> 
        :ok # TODO: log a warning?
    end
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, :process, pid, _},
                  __MODULE__.State[clock: clock, children: c, outstanding: o] = state) do
    [{^pid, child_clock}] = :ets.lookup(c, pid)
    :ets.delete(c, {:ref, ref})
    :ets.delete(c, pid)
    :ets.delete(c, child_clock)
    clock = ITC.join(clock, child_clock)
    State[] = state = state.clock(clock).outstanding(o -- [child_clock])
    state = state.crash_clock(Enum.reduce(state.outstanding, clock, fn(c, c1) -> ITC.join(c, c1) end))
    {:noreply, state}
  end

  @spec execute(pid, Genomu.key | Genomu.cell, Genomu.command, Keyword.t, term, pid) :: :ok
  defcast execute(key, Genomu.Command[] = cmd, options, tag, pid), state: State[] = state do
    if cmd.assertion? and state.modified do
      pid <- {tag, :abort}
      {:noreply, state}
    else
      State[] = state = ensure_snapshot(state)
      clock = next_clock(state.clock, cmd)
      cell = get_cell(key, state)
      revision = ITC.encode_binary(clock)
      cmd = cmd.cell(cell).new_revision(revision)
      coord_options = [for: cmd] |>
                      Keyword.merge(options)
      result = Genomu.Coordinator.run(coord_options)
      State[] = state = memoize(key, cmd, clock, revision, state)
      pid <- {tag, result}
      {:noreply, state.modified((not cmd.assertion?) and cmd.type in [:set, :apply])}
    end
  end

  @spec commit(Genomu.gen_server_ref, Genomu.Transaction.t, term, pid) :: :ok
  defcast commit(Genomu.Transaction[] = txn, tag, pid), state: State[parent: parent, clock: clock, log: log] = state do
    case log do
      [] ->
        stop(self)
        pid <- {tag, :ok}
        {:noreply, state}
      _ ->
        clock = sync(parent, clock)
        txn = txn.clock(clock).log(Enum.reverse(log))
        result = Genomu.Coordinator.run(for: txn)
        update(parent, self, clock)
        stop(self)
        pid <- {tag, result}
        {:noreply, state.clock(clock)}
    end
  end

  @spec discard(Genomu.gen_server_ref, term, pid) :: :ok
  defcast discard(tag, pid), state: state do
    stop(self)
    pid <- {tag, :ok}
    {:noreply, state}
  end

  @spec stop(Genomu.gen_server_ref) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
  end

  @spec wipeout(Genomu.gen_server_ref) :: :ok
  defcast wipeout, state: State[root: _root] = state do
    File.close(state.crash_clock_file)
    _result = File.rm(state.crash_clock_filename)
    Lager.debug "Channel #{inspect _root}: wipeout (#{inspect _result})"
    {:stop, :normal, state}
  end

  def handle_info({:'EXIT', _pid, _reason}, state) do
    {:stop, :normal, state}
  end

  def terminate(_, State[root: false]) do
    :folsom_metrics.notify({{Genomu.Metrics, Channels}, {:dec, 1}})
  end
  def terminate(_, State[crash_clock_file: f]) do
    File.close(f)
    :ok
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