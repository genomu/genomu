defmodule Genomu.Channel do
  use GenServer.Behaviour
  import GenX.GenServer

  alias :itc, as: ITC # TODO: remove when ITC is done

  @spec start :: {:ok, pid} | {:error, reason :: term}
  def start do
    Genomu.Channel.fork Genomu.Interval.Root
  end

  @spec start_link(root :: atom | boolean, parent :: nil | pid | atom, interval :: ITC.t) :: {:ok, pid} | {:error, reason :: term}
  def start_link(false, parent, interval) do
    :gen_server.start_link(__MODULE__, {false, parent, interval}, [])
  end
  def start_link(true, parent, interval) do
    :gen_server.start_link(__MODULE__, {true, parent, interval}, [])
  end
  def start_link(name, parent, interval) do
    :gen_server.start_link({:local, name}, __MODULE__, {true, parent, interval}, [])
  end

  defrecord State, root: false, parent: nil,
                   interval: nil,
                   snapshot: nil, children: nil do

    record_type root: boolean, parent: nil | pid | atom,
                interval: nil | ITC.t,
                snapshot: nil | :ets.tid, children: nil | :ets.tid

    def blank(opts) do
      snapshot = :ets.new(__MODULE__.Snapshot, [:ordered_set])
      children = :ets.new(__MODULE__.Children, [:ordered_set])
      new(Keyword.merge([snapshot: snapshot, children: children], opts))
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
        [{^key, i}] -> ITC.encode(i) |> Genomu.Utils.pad_bitstring(8)
      end
    end

  end

  def init({root, parent, interval}) do
    {:ok, State.blank(root: root, parent: parent, interval: interval)}
  end

  @spec interval(pid | atom) :: ITC.t
  defcall interval, state: State[interval: i] = state do
    {:reply, i, state}
  end

  @spec root?(pid | atom) :: boolean
  defcall root?, state: State[root: root] = state do
    {:reply, root, state}
  end

  @spec fork(pid | atom) :: ITC.t
  @spec fork(pid | atom, root :: atom | boolean) :: ITC.t
  def fork(server), do: fork(server, false)

  defcall fork(root), state: State[interval: i, children: c] = state do
    {new_interval, channel_interval} = ITC.fork(i)
    {:ok, channel} = :supervisor.start_child(Genomu.Sup.Channels, [root, self, channel_interval])
    Process.monitor(channel)
    :ets.insert(c, [{channel, channel_interval},
                    {channel_interval, channel}])
    state = state.interval(new_interval)
    {:reply, {:ok, channel}, state}
  end

  defcast update(channel, new_interval), state: State[children: c] = state do
    case :ets.lookup(c, channel) do
      [{^channel, interval}] ->
        :ets.insert(c, [{channel, new_interval}, {new_interval, channel}])
        :ets.delete(c, interval)
      [] -> 
        :ok # TODO: log a warning?
    end
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, _},
                  __MODULE__.State[interval: i, children: c] = state) do
    [{^pid, child_interval}] = :ets.lookup(c, pid)
    :ets.delete(c, pid)
    :ets.delete(c, child_interval)
    new_interval = ITC.join(i, child_interval)
    state = state.interval(new_interval)
    {:noreply, state}
  end  

  @spec execute(pid, Genomu.key, Genomu.command, Keyword.t) :: term
  defcall execute(key, cmd, options), state: State[] = state do
    interval = next_interval(state.interval, cmd)
    cell = {key, state.lookup(key)}
    revision = ITC.encode(interval) |> Genomu.Utils.pad_bitstring(8)
    coordination_options = Keyword.merge([cell: cell,
                                          command: cmd,
                                          revision: revision], options)
    result = Genomu.Coordinator.run(coordination_options)
    state = memoize(key, cmd, interval, state)
    {:reply, result, state}
  end

  @spec commit(pid | atom) :: :ok | {:error, reason :: term}
  defcall commit, state: State[parent: parent, interval: i] = state do
    update(parent, self, i)
    {:reply, :ok, state}
  end

  @spec stop(pid | atom) :: :ok
  defcast stop, state: state do
    {:stop, :normal, state}
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