defmodule Genomu.Coordinator do

  defmacrop default_timeout, do: 1000 # TODO: 60000

  @type option  :: {:timeout, timeout} |
                   {:for, term}

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

  defrecord Quorum, ref: nil, preflist: nil, vnodes: :any, n: nil, r: nil do
    record_type ref: Genomu.Coordinator.ref, preflist: :riak_core_apl.preflist,
                vnodes: :any | :primary,
                n: pos_integer, r: non_neg_integer
  end

  @quorum_ref_index Quorum.__index__(:ref)

  defrecord State, from: nil, for: nil, handler_state: nil,
                   quorums: [],
                   timeout: nil,
                   touched_at: nil, elapsed: 0 do

    record_type    from: {pid, tag :: term}, for: term, handler_state: term,
                   quorum: [Genomu.Coordinator.Quorum.t],
                   timeout: timeout,
                   touched_at: non_neg_integer, elapsed: non_neg_integer

    @spec touch(t) :: t
    def touch(Genomu.Coordinator.State[] = state) do
      state.touched_at(Genomu.Utils.now_in_microseconds)
    end

 end

  @spec default_options :: Keyword.t
  defp default_options do
    [timeout: default_timeout]
  end

  alias Genomu.Coordinator.Protocol, as: Proto
  def init(opts) do
    opts  = Keyword.merge(default_options, opts)
    {:ok, hstate} = Proto.init(opts[:for])
    state = State.new(Keyword.merge(opts, handler_state: hstate)).touch
    {:ok, :init, state}
  end

  defevent init/execute, export: [timeout: :infinity], sync: true, from: from, 
                         state: State[] = state do
    {:next_state, :prepare, state.update(from: from), 0}
  end

  defevent prepare/timeout, state: State[for: for, handler_state: hstate] = state do
    {:ok, quorums, hstate} = Proto.quorums(for, hstate)
    {:next_state, :execute, state.update(handler_state: hstate, quorums: quorums), 0}
  end

  defevent execute/timeout, state: State[quorums: quorums, 
                                         for: for, handler_state: hstate] = state do
    hstate = Enum.reduce quorums, hstate, 
                         fn(quorum, hstate) ->
                           {:ok, message, hstate} = Proto.message(for, quorum, hstate)
                           :riak_core_vnode_master.command(quorum.preflist, message, 
                                                          {:fsm, :undefined, self},
                                                          Genomu.VNode_master)
                           hstate
                         end
    {:next_state, :waiting, state.handler_state(hstate), 0}
  end


  def waiting(:timeout, State[] = state) do
    case time_exceeded?(state) do
      true ->
        done(:timeout, state)
      {false, state, timeout} ->
        {:next_state, :waiting, state, timeout}
    end
  end

  def waiting({:ok, ref, partition, response},
              State[quorums: quorums,
                    for: for, handler_state: hstate] = state) do
    quorum = List.keyfind(quorums, ref, @quorum_ref_index)
    case Proto.handle_response(for, response, partition, quorum, hstate) do
      {:ok, quorum, hstate} ->
         quorum = quorum.update_r(&1 - 1)
      {:ignore, quorum, hstate} -> 
         :ok
    end
    quorums = List.keyreplace(quorums, ref, @quorum_ref_index, quorum)
    state = state.update(handler_state: hstate, quorums: quorums)
    if all_quorums_ready?(quorums) do
      case Proto.finalize(for, hstate) do
        {:reply, response, hstate} ->
          done(response, state.handler_state(hstate))
        {:ok, hstate} ->
          state = state.handler_state(hstate)
          case time_exceeded?(state) do
            true ->
              done(:timeout, state)
            {false, state, timeout} ->
              {:next_state, :waiting, state, timeout}
          end
      end
    else
      case time_exceeded?(state) do
        true ->
          done(:timeout, state)
        {false, state, timeout} ->
          {:next_state, :waiting, state, timeout}
      end
    end
  end

  @spec all_quorums_ready?([Quorum.t]) :: boolean
  def all_quorums_ready?(quorums) do
    not Enum.any?(quorums, fn(Quorum[r: r]) -> r > 0 end)
  end

  @spec done(response :: term, State.t) :: {:stop, :normal, State.t}
  defp done(response, State[from: from] = state) do
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


end