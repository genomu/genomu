defmodule Genomu.Protocol do

  use GenServer.Behaviour

  def start_link(listener_pid, socket, transport, opts) do
    :proc_lib.start_link(__MODULE__, :init, [listener_pid, socket, transport, opts])
  end

  defrecord State, socket: nil, transport: nil,
                   channels: nil

  def init(listener_pid, socket, transport, _opts) do
    :folsom_metrics.notify({{Genomu.Metrics, Connections}, {:inc, 1}})
    :erlang.process_flag(:trap_exit, true)
    :ok = :proc_lib.init_ack({:ok, self})
    :ok = :ranch.accept_ack(listener_pid)
    :ok = transport.setopts(socket, active: true, packet: 4)
    state = State.new(socket: socket, transport: transport,
                      channels: :ets.new(__MODULE__.Channels, [:ordered_set]))
    :gen_server.enter_loop(__MODULE__, [], state)
  end

  def handle_info({:tcp, socket, data}, State[socket: socket] = state) do
    state = handle_packet(data, state)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, State[socket: socket] = state) do
    {:stop, :normal, state}
  end

  def handle_info({:'EXIT', pid, _reason}, State[channels: channels] = state) do
    case :ets.lookup(channels, pid) do
      [] ->
        {:stop, :normal, state}
      [{_, channel}] ->
        # Remove the channel and continue on
        :ets.delete(channels, pid)
        :ets.delete(channels, channel)
        {:noreply, state}
    end
  end

  def handle_cast({:subscription, channel, subscription, commit_object},
                  State[socket: socket, transport: transport] = state) do
    transport.send(socket, channel <> MsgPack.pack(subscription) <> MsgPack.pack(commit_object))
    {:noreply, state}
  end

  def handle_cast({channel, response}, State[channels: channels, transport: transport, socket: socket] = state) do
    t1 = Genomu.Utils.now_in_microseconds
    [{_, t}] = :ets.lookup(channels, {:t, channel})
    :ets.delete(channels, {:t, channel})
    :folsom_metrics.notify({Genomu.Metrics, ChannelResponseTime}, t1 - t)
    transport.send(socket, channel <> handle_response(response))
    {:noreply, state}
  end

  @true_value MsgPack.pack(true)
  @nil_value MsgPack.pack(nil)
  @txn_value Genomu.Transaction.new
  @cmd_value Genomu.Command.new

  @spec handle_packet(binary, State.t) :: {State.t, binary}
  defp handle_packet(data, State[channels: channels] = state) do
    {channel, rest} = MsgPack.next(data)
    me = self
    case :ets.lookup(channels, channel) do
      [] ->
        case MsgPack.unpack(rest) do
          {MsgPack.Map[map: options], rest} ->
            {:ok, ch} = Genomu.Channel.start
            Process.link(ch)
            templates = {set_txn_options(options, @txn_value),
                         set_command_options(options, @cmd_value)}
            :ets.insert(channels, [{channel, ch, templates}, {ch, channel}])
          {subscriptions, rest} when is_list(subscriptions) ->
            {:ok, ch} = Genomu.Watcher.start(fn(subscription, co) ->
                                               :gen_server.cast(me, {:subscription, channel, subscription, co})
                                             end, subscriptions)
            Process.link(ch)
            :ets.insert(channels, [{channel, ch, subscriptions}, {ch, channel}])
        end
      [{_, ch, {txn_t, cmd_t}}] ->
        {key, rest} = MsgPack.unpack(rest)
        :ets.insert(channels, {{:t, channel}, Genomu.Utils.now_in_microseconds})
        case key do
          true ->
             spawn(fn ->
                response = Genomu.Channel.commit(ch, txn_t)
                :gen_server.cast(me, {channel, response})
              end)
          false ->
             spawn(fn ->
                response = Genomu.Channel.discard(ch)
                :gen_server.cast(me, {channel, response})
              end)
          _ ->
            case key do
              MsgPack.Map[map: [{"vsn", [key, rev]}]] -> addr = {key, rev}
              addr -> :ok
            end
            {type, op} = MsgPack.unpack(rest)
            cmd = command(type, op, cmd_t)
            spawn(fn ->
                    response = Genomu.Channel.execute(ch, addr, cmd, [])
                    :gen_server.cast(me, {channel, response})
                  end)
        end
    end
    state
  end

  defp handle_response(:ok) do
    @true_value
  end
  defp handle_response(:timeout) do
    @nil_value <> MsgPack.pack(1)
  end
  defp handle_response(:abort) do
    @nil_value <> MsgPack.pack(0)
  end
  defp handle_response({{value, clock}, txn}) do
    value <> MsgPack.pack(clock) <> MsgPack.pack(txn)
  end

  def terminate(_, _state) do
      :folsom_metrics.notify({{Genomu.Metrics, Connections}, {:dec, 1}})
  end

  alias Genomu.Constants.ChannelOptions, as: CO
  require CO

  defp command(0, op, Genomu.Command[] = cmd), do: cmd.type(:get).operation(op)
  defp command(1, op, Genomu.Command[] = cmd), do: cmd.type(:set).operation(op)
  defp command(2, op, Genomu.Command[] = cmd), do: cmd.type(:apply).operation(op)

  defp set_command_options([], cmd), do: cmd
  defp set_command_options([{CO.n, n}|t], Genomu.Command[] = cmd) do
    set_command_options(t, cmd.n(n))
  end
  defp set_command_options([{CO.r, r}|t], Genomu.Command[] = cmd) do
    set_command_options(t, cmd.r(r))
  end
  defp set_command_options([{CO.vnode, 0}|t], Genomu.Command[] = cmd) do
    set_command_options(t, cmd.vnode(:any))
  end
  defp set_command_options([{CO.vnode, 1}|t], Genomu.Command[] = cmd) do
    set_command_options(t, cmd.vnode(:primary))
  end
  defp set_command_options([{CO.timeout, timeout}|t], Genomu.Command[] = cmd) do
    set_command_options(t, cmd.timeout(timeout))
  end

  defp set_txn_options([], txn), do: txn
  defp set_txn_options([{CO.n, n}|t], Genomu.Transaction[] = txn) do
    set_txn_options(t, txn.n(n))
  end
  defp set_txn_options([{CO.r, r}|t], Genomu.Transaction[] = txn) do
    set_txn_options(t, txn.r(r))
  end
  defp set_txn_options([{CO.vnode, 0}|t], Genomu.Transaction[] = txn) do
    set_txn_options(t, txn.vnode(:any))
  end
  defp set_txn_options([{CO.vnode, 1}|t], Genomu.Transaction[] = txn) do
    set_txn_options(t, txn.vnode(:primary))
  end
  defp set_txn_options([{CO.timeout, timeout}|t], Genomu.Transaction[] = txn) do
    set_txn_options(t, txn.timeout(timeout))
  end

end