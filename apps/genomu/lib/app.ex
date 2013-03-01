import Xup
defsupervisor Genomu.Sup do

  worker id: :riak_core_app,
         function: :start, args: [:undefined, :undefined],
         modules: :dynamic

  worker id: Genomu.VNode,
         module: :riak_core_vnode_master, args: [Genomu.VNode],
         modules: [:riak_core_vnode_master, Genomu.VNode]

  supervisor Coordinator, strategy: :simple_one_for_one do
    worker id: Worker, restart: :temporary,
           module: Genomu.Coordinator
  end

  worker id: Genomu.Channel.Root,
         module: Genomu.Channel, args: [Genomu.Channel.Root, nil, ITC.seed],
         restart: :permanent

  supervisor Channels, strategy: :simple_one_for_one do
    worker id: Genomu.Channel, restart: :temporary
  end


end

defmodule Genomu.App do
  use Application.Behaviour

  def start(_, _) do
    case Genomu.Sup.start_link do
      {:ok, pid} ->
        :ok = :riak_core.register([{:vnode_module, Genomu.VNode}])
        :ok = :riak_core_node_watcher.service_up(Genomu.VNode, pid)
        setup_modules
        {:ok, pid}
      other -> other
    end
  end

  defp setup_modules do
    quoted =
    Enum.map(modules, fn(m) ->
      quote do
        def :module, [Genomu.Module.id(unquote(m))], [], do: unquote(m)
      end
    end)
    Module.create Genomu.Commands, quoted, __ENV__
  end

  defp modules, do: [Genomu.Module.Core, Genomu.Module.Binary]

end
