defmodule Genomu.Coordinator do

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
                   timeout: 5000,
                   sent_at: nil, started_at: nil,
                   touched_at: nil, elapsed: 0 do

    record_type    from: {pid, tag :: term}, for: term, handler_state: term,
                   quorum: [Genomu.Coordinator.Quorum.t],
                   timeout: timeout,
                   sent_at: non_neg_integer, started_at: non_neg_integer,
                   touched_at: non_neg_integer, elapsed: non_neg_integer

    @spec touch(t) :: t
    def touch(Genomu.Coordinator.State[] = state) do
      state.touched_at(Genomu.Utils.now_in_microseconds)
    end

 end

  alias Genomu.Coordinator.Protocol, as: Proto
  def init(opts) do
    for = opts[:for]
    {:ok, hstate} = Proto.init(for)
    hopts = Proto.options(for)
    state = State[timeout: hopts[:timeout] || State[].timeout, for: for].handler_state(hstate).touch
    {:ok, :init, state}
  end

  defevent init/execute, export: [timeout: :infinity], sync: true, from: from, 
                         state: State[] = state do
    {:next_state, :prepare, state.from(from).started_at(Genomu.Utils.now_in_microseconds), 0}
  end

  defevent prepare/timeout, state: State[for: for, handler_state: hstate] = state do
    {:ok, quorums, hstate} = Proto.quorums(for, hstate)
    {:next_state, :execute, state.handler_state(hstate).quorums(quorums), 0}
  end

  defevent execute/timeout, state: State[quorums: quorums, 
                                         for: for, handler_state: hstate] = state do
    hstate = Enum.reduce quorums, hstate, 
                         fn(Quorum[preflist: preflist] = quorum, hstate) ->
                           {:ok, message, hstate} = Proto.message(for, quorum, hstate)
                           Genomu.Quorum.command(preflist, message,
                                                 {:fsm, :undefined, self})
                           hstate
                         end
    {:next_state, :waiting, state.sent_at(Genomu.Utils.now_in_microseconds).handler_state(hstate), 0}
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
                    for: for, handler_state: hstate, sent_at: sent_at] = state) do
    case time_exceeded?(state) do
      true ->
        done(:timeout, state)
      {false, State[] = state, timeout} ->
        :folsom_metrics.notify({Genomu.Metrics, PartitionResponseTime}, Genomu.Utils.now_in_microseconds - sent_at)
        quorum = List.keyfind(quorums, ref, @quorum_ref_index)
        case Proto.handle_response(for, response, partition, quorum, hstate) do
          {:ok, Quorum[r: r] = quorum, hstate} ->
             quorum = quorum.r(r - 1)
          {:ignore, quorum, hstate} -> 
             :ok
        end
        quorums = List.keyreplace(quorums, ref, @quorum_ref_index, quorum)
        State[] = state = state.handler_state(hstate).quorums(quorums)
        if all_quorums_ready?(quorums) do
          :folsom_metrics.notify({Genomu.Metrics, QuorumTime}, Genomu.Utils.now_in_microseconds - sent_at)
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
  end

  @spec all_quorums_ready?([Quorum.t]) :: boolean
  def all_quorums_ready?(quorums) do
    not Enum.any?(quorums, fn(Quorum[r: r]) -> r > 0 end)
  end

  @spec done(response :: term, State.t) :: {:stop, :normal, State.t}
  defp done(response, State[from: from, started_at: started_at] = state) do
    :gen_fsm.reply(from, response)
    :folsom_metrics.notify({Genomu.Metrics, CoordinationTime}, Genomu.Utils.now_in_microseconds - started_at)
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