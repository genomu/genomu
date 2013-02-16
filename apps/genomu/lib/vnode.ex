 defmodule Genomu.VNode do
   @behaviour :riak_core_vnode
   alias :riak_core_vnode_master, as: Master
   alias :riak_core_vnode, as: VNode
   alias :ets, as: ETS

   @type partition :: non_neg_integer

   @type sender :: {:fsm | :server | :raw, reference, pid} |
                   {:server, :undefined, :undefined} |
                   {:fsm, :undefined, pid} |
                   :ignore

   @type command :: {Genomu.cell, ITC.t, Genomu.command,
                     Genomu.Coordinator.ref}

   require Lager

   defrecord State, partition: nil,
                    tab: nil do
     record_type    partition: Genomu.VNode.partition,
                    tab: :ets.tab
   end

   @spec start_vnode(non_neg_integer) :: {:ok, pid}
   def start_vnode(partition) do
     Master.get_vnode_pid(partition, __MODULE__)
   end

   @spec init([non_neg_integer]) :: {:ok, State.t}
   def init([partition]) do
     :erlang.process_flag(:trap_exit, true)
     tab = ETS.new(__MODULE__, [:ordered_set])
     {:ok, State.new(partition: partition, tab: tab)}
   end

   @spec handle_command(command, sender, State.t) ::
         {:reply, term, State.t} |
         {:noreply, State.t} |
         {:async, sender, term, State.t} |
         {:stop, term, State.t}
   def handle_command({{key, _rev} = cell, interval, cmd, ref}, sender,
                      State[tab: tab] = state) do
     case ETS.lookup(tab, cell) do
       [] -> value = nil
       [{^cell, {_operation, value}}] -> :ok
     end
     case cmd do
       {cmd_name, operation} when cmd_name in [:apply, :set] ->
         new_value = Genomu.Operation.apply(operation, value)
         serialized = Genomu.Operation.serialize(operation)
         ETS.insert(tab, {{key, interval}, {serialized, new_value}})
       {:get, operation} ->
         new_value = Genomu.Operation.apply(operation, value)
     end
     unless cmd_name == :apply do
       VNode.reply(sender, {:ok, ref, state.partition, new_value})
     else
       VNode.reply(sender, {:ok, ref, state.partition, :ok})
     end
     {:noreply, state}
   end


   @spec handle_handoff_command(term, sender, State.t) ::
         {:reply, term, State.t} |
         {:noreply, State.t} |
         {:async, sender, term, State.t} |
         {:stop, term, State.t} |
         {:forward, State.t} |
         {:drop, State.t}
   def handle_handoff_command(_, _sender, State[] = state) do
     {:forward, state}
   end

   def handoff_starting(_target, State[] = state) do
     {true, state}
   end

   def handoff_cancelled(State[] = state), do: {:ok, state}

   def handoff_finished(_target, State[] = state), do: {:ok, state}

   def handle_handoff_data(data, State[tab: tab] = state) do
     {key, value} = binary_to_term(data)
     ETS.insert(tab, {key, value})
     {:reply, :ok, state}
   end

   def encode_handoff_item(key, value) do
     term_to_binary({key, value})
   end

   def is_empty(State[tab: tab] = state) do
     info = ETS.info(tab)
     {info[:size] == 0, state}
   end

   def delete(State[tab: tab] = state) do
     ETS.delete_all_objects(tab)
     {:ok, state}
   end

   def handle_coverage(_, _filter_vnodes, _sender, State[] = state) do
     {:reply, :done, state}
   end


   def handle_exit(_pid, _reason, State[] = state) do
     {:noreply, state}
   end

   def terminate(_, State[]) do
   end

 end