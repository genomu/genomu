defmodule Genomu.Coordinator do

  defmacrop default_N, do: 3
  defmacrop default_R, do: 2
  defmacrop default_timeout, do: 1000 # TODO: 60000

  @type option  :: {:n, pos_integer} |
                   {:r, pos_integer} |
                   {:dr, non_neg_integer} |
                   {:timeout, timeout} |
                   {:vnodes, :any | :primary} |
                   {:cell, Genomu.VNode.cell} |
                   {:command, Genomu.command} |
                   {:interval, ITC.t}

  @type options :: [option]
  @type index :: binary

  @type ref :: reference

  use GenFsm.Behaviour
  import GenX.GenFsm

  @spec start(options) :: {:ok, pid} | {:error, reason :: term}
  def start(opts) do
    :supervisor.start_child(Genomu.Sup.Coordinator, [opts])
  end

  @spec run(options) :: term
  def run(opts) do
    {:ok, pid} = start(opts)
    execute(pid)
  end

  @spec start_link(options) :: {:ok, pid} | {:error, reason :: term}
  def start_link(opts) do
    :gen_fsm.start_link(__MODULE__, opts, [])
  end

  defrecord State, preflist: nil,
                   vnodes: :any,
                   command: nil, cell: nil,
                   interval: nil,
                   n: nil, r: nil, dr: nil,
                   timeout: nil,
                   ref: nil, from: nil,
                   acc: [],
                   touched_at: nil, elapsed: 0 do

    record_type    preflist: :riak_core_apl.preflist,
                   vnodes: :any | :primary,
                   command: Genomu.command,
                   cell: Genomu.cell, interval: ITC.t,
                   n: pos_integer, r: non_neg_integer, dr: non_neg_integer,
                   timeout: timeout,
                   ref: Genomu.ref, from: {pid, tag :: term} | nil,
                   acc: list,
                   touched_at: non_neg_integer, elapsed: non_neg_integer

    @spec touch(t) :: t
    def touch(Genomu.Coordinator.State[] = state) do
      state.touched_at(Genomu.Utils.now_in_microseconds)
    end

 end

  @spec default_options :: Keyword.t
  defp default_options do
    [n: default_N, r: default_R, timeout: default_timeout, vnodes: :any]
  end
  @spec default_interpolated_options(Keyword.t) :: Keyword.t
  defp default_interpolated_options(options) do
    [dr: options[:r] || default_R]
  end

  def init(opts) do
    opts  = Keyword.merge(default_options, opts)
    opts  = Keyword.merge(default_interpolated_options(opts), opts)
    state = State.new(opts).touch
    {:ok, :init, state}
  end

  defevent init/execute, export: [timeout: :infinity], sync: true, from: from, state: State[] = state do
    {:next_state, :prepare, state.update(ref: make_ref, from: from), 0}
  end

  defevent prepare/timeout, state: State[] = state do
    preflist = get_preflist(state)
    {:next_state, :execute, state.preflist(preflist), 0}
  end

  defevent execute/timeout, state: State[] = state do
    :riak_core_vnode_master.command(state.preflist,
                                    {state.cell, state.interval, state.command, state.ref},
                                    {:fsm, :undefined, self},
                                    Genomu.VNode_master)
    {:next_state, :waiting, state, 0}
  end


  def waiting(:timeout, State[from: from, r: 0, dr: 0] = state) do
    done(from, :ok, state)
  end

  def waiting(:timeout, State[from: from] = state) do
    case time_exceeded?(state) do
      true ->
        done(from, :timeout, state)
      {false, state, timeout} ->
        {:next_state, :waiting, state, timeout}
    end
  end

  def waiting({:ok, ref, partition, response},
              State[ref: ref, from: from, acc: acc,
                    r: r] = state) do
    r = r - 1
    acc = [{partition, response}|acc]
    state = state.update(r: r, acc: acc)
    if r == 0 do
      [first_value|_] = values =
        lc {_partition, value} inlist acc, do: value
      if Enum.any?(values, fn(v) -> v != first_value end) do
        done(from, {:conflict, values}, state)
      else
        done(from, first_value, state)
      end
    else
      case time_exceeded?(state) do
        true ->
          done(from, :timeout, state)
        {false, state, timeout} ->
          {:next_state, :waiting, state, timeout}
      end
    end
  end

  @spec done(from :: {pid, tag :: term}, response :: term, State.t) :: {:stop, :normal, State.t}
  defp done(from, response, state) do
    :gen_fsm.reply(from, response)
    :supervisor.terminate_child(Genomu.Sup.Coordinator, self)
    {:stop, :normal, state}
  end

  @spec time_exceeded?(State.t) :: true | {false, State.t, timeout}
  defp time_exceeded?(State[timeout: timeout, touched_at: touched_at, elapsed: elapsed] = state) do
    now = Genomu.Utils.now_in_microseconds
    elapsed = elapsed + (now - touched_at)
    if elapsed >= timeout * 1000 do
      true
    else
      timeout = div(timeout  * 1000 - elapsed, 1000)
      {false, state.elapsed(elapsed), timeout}
    end
  end

  @spec get_preflist(State.t) :: :riak_core_apl.preflist

  defp get_preflist(State[cell: {key, _rev}, vnodes: :primary] = state) do
    lc {{partition, node}, _type} inlist :riak_core_apl.get_primary_apl(hash(key), state.n, Genomu.VNode) do
      {partition, node}
    end
  end

  defp get_preflist(State[cell: {key, _rev}] = state) do
    :riak_core_apl.get_apl(hash(key), state.n, Genomu.VNode)
  end

  @spec hash(Genomu.key) :: index
  defp hash(key), do: :crypto.sha(key)

end