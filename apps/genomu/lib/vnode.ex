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

   @type command :: {Genomu.cell, Genomu.revision, Genomu.command,
                     Genomu.Coordinator.ref}
   @type commit :: {:C, ITC.t, binary, [Genomu.Transaction.entry], Genomu.Coordinator.ref}

   require Lager

   defrecord State, partition: nil,
                    tab: nil, staging_tab: nil do
     record_type    partition: Genomu.VNode.partition,
                    tab: :ets.tid, staging_tab: :ets.tid
   end

   @spec start_vnode(non_neg_integer) :: {:ok, pid}
   def start_vnode(partition) do
     Master.get_vnode_pid(partition, __MODULE__)
   end

   @spec init([non_neg_integer]) :: {:ok, State.t}
   def init([partition]) do
     :erlang.process_flag(:trap_exit, true)
     tab = ETS.new(__MODULE__, [:ordered_set])
     staging_tab = ETS.new(__MODULE__.Staging, [:ordered_set])
     {:ok, State.new(partition: partition, tab: tab, staging_tab: staging_tab)}
   end

   @spec handle_command(command | commit, sender, State.t) ::
         {:reply, term, State.t} |
         {:noreply, State.t} |
         {:async, sender, term, State.t} |
         {:stop, term, State.t}
   def handle_command({{key, _rev} = cell, new_rev, cmd, ref}, sender,
                      State[] = state) do
     {value, rev} = lookup_cell(cell, state)
     case cmd do
       {cmd_name, operation} when cmd_name in [:apply, :set] ->
         new_value = Genomu.Operation.apply(operation, value)
         serialized = Genomu.Operation.serialize(operation)
         stage({key, new_rev}, serialized, new_value, state)
       {:get = cmd_name, operation} ->
         new_value = Genomu.Operation.apply(operation, value)
     end
     case cmd_name do
       :apply ->
         VNode.reply(sender, {:ok, ref, state.partition, :ok})
       :get ->
         VNode.reply(sender, {:ok, ref, state.partition, {new_value, rev}})
       :set ->
         VNode.reply(sender, {:ok, ref, state.partition, {new_value, new_rev}})
     end
     {:noreply, state}
   end

   def handle_command({:C, clock, commit_object, entries, ref}, _sender, 
                      State[tab: tab, staging_tab: staging] = state) do
     lc {key, entry_clock} = entry inlist entries do
       case ETS.lookup(tab, key) do
         [] -> history = []
         [{^key, {_value, history}}] -> :ok
       end
       [{^entry, {value, _serialized}}] = ETS.lookup(staging, entry)
       ETS.insert(tab, {key, {value, [{entry_clock, clock}|history]}})
     end
     ETS.insert(tab, {["_","sys","commit", clock], {commit_object, [clock]}})
     {:reply, {:ok, ref, state.partition, :ok}, state}
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

   @nil_value MsgPack.pack(nil)

   @spec lookup_cell(Genomu.cell, State.t) :: term | nil
   defp lookup_cell({key, nil}, State[tab: tab]) do
     case ETS.lookup(tab, key) do
       [{^key, {value, [{clock,_}|_history]}}] -> {value, clock}
       _v -> {@nil_value, ""}
     end
   end
   defp lookup_cell({_key, rev} = cell, State[staging_tab: tab]) do
     case ETS.lookup(tab, cell) do
       [{^cell, {value, _serialized}}] -> {value, rev}
       _ -> {@nil_value, rev}
     end
   end

   @spec stage(Genomu.cell, Genomu.Operation.serialized, value :: term, State.t) :: State.t
   defp stage(cell, serialized, value, State[staging_tab: tab] = state) do
     ETS.insert(tab, {cell, {value, serialized}})
     state
   end

 end