defmodule Genomu.Channel.Root do
  use GenServer.Behaviour
  import GenX.GenServer

  require Lager

  @spec start_link :: {:ok, pid} | {:error, reason :: term}
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  defrecord State, clock: nil, outstanding: [] do
    record_type clock: nil | ITC.t, outstanding: [ITC.t]

    defp crash_clock_filename do
      data_dir = Application.environment(:genomu)[:data_dir]
      Path.join(data_dir, "clock")
    end

    @spec initialize(t) :: t
    def initialize(__MODULE__[] = state) do
      filename = crash_clock_filename
      if File.exists?(filename) do
        {:ok, f} = File.open(filename, [:binary, :raw, :read, :write])
        {:ok, bin} = :file.pread(f, 0, File.stat!(filename).size)
        state.clock(ITC.decode(bin))
      else
        dump_clock(ITC.seed)
        state.clock(ITC.seed)
      end
    end

    def dump_clock(clock) do
      filename = crash_clock_filename
      {:ok, f} = File.open(filename, [:binary, :raw, :read, :write])
      :file.pwrite(f, 0, ITC.encode_binary(clock))
      :file.datasync(f)
      :file.close(f)
    end    
  end

  def init(_) do
    {:ok, State.new.initialize}
  end

  defcall clock, export: [server: __MODULE__], state: State[clock: clock] = state do
    {:reply, clock, state}
  end

  defcall persistent_clock, export: [server: __MODULE__], state: state do
    {:reply, State.new.initialize.clock, state}
  end

  defcall fork, export: [server: __MODULE__], state: State[clock: clock] = state do
    {new_clock, channel_clock} = ITC.fork(clock)
    case :supervisor.start_child(Genomu.Sup.Channels, [channel_clock]) do
      {:ok, channel} ->
        State[] = state = state.outstanding([channel_clock|state.outstanding])
        {:reply, {:ok, channel}, state.clock(new_clock)}
      error ->
        {:reply, error, state}
    end
  end

  defcall fork_clock, state: State[clock: clock] = state do
    {new_clock, clock1} = ITC.fork(clock)
    pclock = Enum.reduce(state.outstanding, new_clock, fn(c, c1) -> ITC.join(c, c1) end)
    State.dump_clock(pclock)
    {:reply, clock1, state.clock(new_clock)}
  end

  defcall join(clock0, clock1), export: [server: __MODULE__], state: State[clock: clock] = state do
    clock = ITC.join(clock, clock1)
    State[] = state = state.outstanding(state.outstanding -- [clock0])
    pclock = Enum.reduce(state.outstanding, clock, fn(c, c1) -> ITC.join(c, c1) end)
    State.dump_clock(pclock)
    {:reply, pclock, state.clock(clock)}
  end

  defcall sync_with(sync_with), state: State[] = state do
    clock = fork_clock(sync_with)
    State[] = state = state.clock(clock).outstanding([])
    State.dump_clock(state.clock)
    {:reply, :ok, state}
  end
end