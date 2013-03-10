import Xup
defsupervisor Genomu.Sup, name: {:local, :genomu_sup} do

  worker id: :riak_core_app,
         function: :start, args: [:undefined, :undefined],
         modules: :dynamic

  worker id: Genomu.Metrics

  worker id: Genomu.VNode,
         module: :riak_core_vnode_master, args: [Genomu.VNode],
         modules: [:riak_core_vnode_master, Genomu.VNode]

  supervisor Coordinator, strategy: :simple_one_for_one do
    worker id: Worker, restart: :temporary,
           module: Genomu.Coordinator
  end

  worker id: Genomu.Channel.Root,
         module: Genomu.Channel, args: [Genomu.Channel.Root, nil, nil],
         restart: :permanent

  supervisor Channels, strategy: :simple_one_for_one do
    worker id: Genomu.Channel, restart: :temporary
  end

  worker id: Genomu.DNSSD


end

defmodule Genomu.App do
  use Application.Behaviour

  def start(_, _) do
    env = Application.environment(:genomu)
    pid_file = env[:pid_file]
    protocol_port = env[:port]
    File.mkdir_p(Path.dirname(pid_file))
    File.write!(pid_file, System.get_pid)
    case Genomu.Sup.start_link do
      {:ok, pid} ->
        :ok = :riak_core.register(:genomu, [{:vnode_module, Genomu.VNode}])
        {:ok, _} = Genomu.HTTP.start
        {:ok, _} = :ranch.start_listener(Genomu.Protocol, 100,
                          :ranch_tcp, [port: protocol_port],
                          Genomu.Protocol, [])
        {:ok, pid}
      other -> other
    end
  end

  def stop(_) do
    env = Application.environment(:genomu)
    pid_file = env[:pid_file]
    File.rm pid_file
  end
end
