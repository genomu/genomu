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
   @type commit :: {:C, Genomu.revision, term, [Genomu.Transaction.entry], Genomu.Coordinator.ref}

   @typep fold_req :: {:riak_core_fold_req_v1, ((term, term) -> term), term}

   @nil_value MsgPack.pack(nil)

   require Lager
   require Genomu.Constants.CommitObject
   alias Genomu.Constants.CommitObject, as: CO

   alias Genomu.Storage, as: S

   defrecord State, partition: nil, storage: nil do
     record_type    partition: Genomu.VNode.partition,
                    storage: Genomu.Storage.t
   end

   @spec start_vnode(integer | [integer]) :: {:ok, pid}
   def start_vnode(partition) do
     Master.get_vnode_pid(partition, __MODULE__)
   end

   @spec init([non_neg_integer]) :: {:ok, State.t}
   def init([partition]) do
     :erlang.process_flag(:trap_exit, true)
     {:ok, s} = Genomu.Storage.init(Application.environment(:genomu)[:storage], [partition: partition])
     {:ok, State.new(storage: s, partition: partition)}
   end

   @spec handle_command(fold_req | command | commit, sender, State.t) ::
         {:reply, term, State.t} |
         {:noreply, State.t} |
         {:async, sender, term, State.t} |
         {:stop, term, State.t}

   def handle_command({:riak_core_fold_req_v1, foldfun, acc0}, _sender, 
                              State[storage: s] = state) do
     acc = S.reduce(s, acc0, foldfun)   
     {:reply, acc, state}
   end         

   def handle_command({{key, _rev} = cell, new_rev, cmd, ref}, sender,
                      State[storage: s] = state) do
     {value, op, rev, txn_ref} = S.lookup(s, cell)
     case cmd do
       {:get, operation} ->
         case apply_operation(operation, value) do
           :error ->
             new_value = @nil_value
           new_value ->
             :ok
         end
         VNode.reply(sender, {:ok, ref, state.partition, {{new_value, rev}, txn_ref}})
       {:operation, nil} ->
         VNode.reply(sender, {:ok, ref, state.partition, {{MsgPack.pack(op), rev}, txn_ref}})
       {cmd_name, operation} ->
         case apply_operation(operation, value) do
           :error ->
             new_rev = rev
             new_value = @nil_value
           new_value ->
            S.stage(s, {key, new_rev}, operation, new_value)
         end
         case cmd_name do
           :apply ->
             VNode.reply(sender, {:ok, ref, state.partition, :ok})
           :set ->
             VNode.reply(sender, {:ok, ref, state.partition, {{new_value, new_rev}, txn_ref}})
         end
     end
     {:noreply, state}
   end

   def handle_command({:C, revision, commit_object, entries, ref}, _sender, 
                      State[storage: s] = state) do
     MsgPack.Map[map: commit_object_dict] = commit_object
     MsgPack.Map[map: log] = commit_object_dict[CO.log]
     S.commit(s, revision, entries, log, MsgPack.pack(commit_object))
     {:reply, {:ok, ref, state.partition, :ok}, state}
   end

   @spec handle_handoff_command(term, sender, State.t) ::
         {:reply, term, State.t} |
         {:noreply, State.t} |
         {:async, sender, term, State.t} |
         {:stop, term, State.t} |
         {:forward, State.t} |
         {:drop, State.t}
   def handle_handoff_command({:riak_core_fold_req_v1, _, _} = fold_req, sender, state) do
     handle_command(fold_req, sender, state)
   end
   def handle_handoff_command(_, _sender, State[] = state) do
     {:forward, state}
   end

   def handoff_starting(_target, State[] = state) do
     {true, state}
   end

   def handoff_cancelled(State[] = state), do: {:ok, state}

   def handoff_finished(_target, State[] = state), do: {:ok, state}

   def handle_handoff_data(data, State[storage: s] = state) do
     S.unpack(s, binary_to_term(data))
     {:reply, :ok, state}
   end

   def encode_handoff_item(key, value) do
     term_to_binary({key, value})
   end

   def is_empty(State[storage: s] = state) do
     {S.size(s) == 0, state}
   end

   def delete(State[storage: s] = state) do
     S.delete(s)
     {:ok, state}
   end

   def handle_coverage(_, _filter_vnodes, _sender, State[] = state) do
     {:reply, :done, state}
   end


   def handle_exit(_pid, _reason, State[] = state) do
     {:noreply, state}
   end

   def terminate(_, State[storage: s]) do
     S.close(s)
   end

   def apply_operation(operation, value) do
     try do
       Genomu.Operation.apply(operation, value)
     catch type, msg -> 
       Lager.error "Error #{inspect {type, msg}} occurred while processing operation #{inspect operation} on #{inspect value}"
       :error
     end
   end

 end