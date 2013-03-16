defmodule Genomu.Sup do

  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, :genomu_sup}, __MODULE__, [])
  end

  def init(_) do
    supervise(tree, strategy: :one_for_one)
  end

  defmodule Coordinator do
    use Supervisor.Behaviour

    def start_link do
      :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def init(_) do
      supervise(tree, strategy: :simple_one_for_one)
    end

    defp tree do
      [ worker(Genomu.Coordinator, [], id: Worker, restart: :temporary) ]
    end
  end

  defmodule Channels.Root do
    use Supervisor.Behaviour

    def start_link do
      :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def init(_) do
      supervise(tree, strategy: :one_for_one)
    end

    defp tree do
      root_channels = tuple_to_list(:mochiglobal.get(Genomu.Channel.RootChannels))
      lc ch inlist root_channels do
        worker(Genomu.Channel, [ch, Genomu.Channel.Root, nil], id: ch, restart: :permanent)
      end
    end
  end

  defmodule Channels do
    use Supervisor.Behaviour

    def start_link do
      :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def init(_) do
      supervise(tree, strategy: :simple_one_for_one)
    end

    defp tree do
      [ worker(Genomu.Channel, [], id: Worker, restart: :temporary) ]
    end
  end

  defmodule Watchers do
    use Supervisor.Behaviour

    def start_link do
      :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
    end

    def init(_) do
      supervise(tree, strategy: :simple_one_for_one)
    end

    defp tree do
      [ worker(Genomu.Watcher, [], id: Worker, restart: :temporary) ]
    end
  end

  defp tree do
    [
      worker(:riak_core_app, [:undefined, :undefined], function: :start),
      worker(Genomu.Metrics, []),
      worker(:riak_core_vnode_master, [Genomu.VNode], id: Genomu.VNode,
             modules: [:riak_core_vnode_master, Genomu.VNode]),
      supervisor(Coordinator, []),
      worker(Genomu.Channel, [Genomu.Channel.Root, nil, nil], id: Genomu.Channel.Root, restart: :permanent),
      supervisor(Channels.Root, []),
      supervisor(Channels, []),
      supervisor(Watchers, []),
      worker(Genomu.DNSSD, []),
     ]
  end

end

defmodule Genomu.App do
  use Application.Behaviour

  def start(_, _) do
    env = Application.environment(:genomu)
    pid_file = env[:pid_file]
    protocol_port = env[:port]
    File.mkdir_p(Path.dirname(pid_file))
    File.write!(pid_file, System.get_pid)

    # Detect host ID
    {:ok, ifaces} = :inet.getifaddrs
    host_id = Enum.find_value(ifaces, fn({_iface, opts}) ->
                              if opts[:hwaddr] do
                                list_to_binary(opts[:hwaddr])
                              end
                            end)
    :mochiglobal.put(Genomu.Utils.HostID, host_id)
    #

    :mochiglobal.put(Genomu.Channel.NRootChannels, env[:root_channels])
    roots =
    Enum.map(1..env[:root_channels], fn(n) ->
      Module.concat([Genomu.Channel.Root, :"N#{n}"])
    end) |> list_to_tuple
    :mochiglobal.put(Genomu.Channel.RootChannels, roots)

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
