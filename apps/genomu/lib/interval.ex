defmodule Genomu.Interval.Server do
  defmacro __using__(_) do
    quote do
      alias :itc, as: ITC # TODO: remove when ITC is done
      def handle_call(:fork, _, __MODULE__.State[interval: i] = state) do
        ITC.fork(i)
        {i1, i2} = ITC.fork(i)
        {:reply, i2, state.interval(i1)}
      end

      def handle_call(:interval, _, __MODULE__.State[interval: i] = state) do
        {:reply, i, state}
      end
    end
  end
end

defmodule Genomu.Interval do
  use GenServer.Behaviour
  import GenX.GenServer

  alias :itc, as: ITC # TODO: remove when ITC is done

  @spec start_link :: {:ok, pid} | {:error, reason :: term}
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, ITC.seed, [])
  end

  @spec start_link(pid | atom) :: {:ok, pid} | {:error, reason :: term}
  def start_link(parent) do
    interval = fork(parent)
    :gen_server.start_link(__MODULE__, interval, [])
  end

  @spec start(pid | atom) :: {:ok, pid} | {:error, reason :: term}
  def start(parent) do
    :supervisor.start_child(Genomu.Sup.Intervals, [parent])
  end

  defrecord State, interval: nil do
    record_type interval: ITC.t
  end

  use Genomu.Interval.Server

  def init(interval) do
    {:ok, State.new(interval: interval)}
  end

  @spec fork(pid | atom) :: ITC.t
  @spec fork :: ITC.t

  def fork(server), do: :gen_server.call(server, :fork)
  def fork, do: fork(__MODULE__)

  @spec interval(pid | atom) :: ITC.t
  @spec interval :: ITC.t

  def interval(server), do: :gen_server.call(server, :interval)
  def interval, do: interval(__MODULE__)

  @spec seed?(pid | atom) :: boolean
  @spec seed? :: boolean

  defcall seed?, state: State[interval: i] = state do
    {:reply, ITC.seed?(i), state}
  end

  def seed?, do: seed?(__MODULE__)

end