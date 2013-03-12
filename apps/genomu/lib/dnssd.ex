defmodule Genomu.DNSSD do

  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  defrecord State, registration: nil, browser: nil, instances: nil

  def init(_) do
    :erlang.process_flag(:trap_exit, true)
    :gen_server.cast(self, :announce)
    :gen_server.cast(self, :browse)
    {:ok, State.new(instances: :ets.new(__MODULE__, []))}
  end

  alias :dnssd, as: SD

  def available_instances do
    :gen_server.call(__MODULE__, :available_instances)
  end

  def handle_call(:available_instances, _from, State[instances: instances] = state) do
    result = Enum.filter_map(:ets.match(instances, {:_, :'$1'}), fn([v]) -> not is_tuple(v) end, fn([v]) -> v end) |>
             Enum.uniq
    {:reply, result, state}
  end

  def handle_cast(:announce, State[] = state) do
    env = Application.environment(:genomu)
    {:ok, ref} =
    SD.register("#{Genomu.instance_name} @ #{Genomu.instance_url}",
                "_genomu._tcp", env[:port],
                [
                  name: Genomu.instance_name,
                  system_version: Genomu.system_version,
                  url: Genomu.instance_url,
                  cluster_name: Genomu.Cluster.name,
                ])
    {:noreply, state.registration(ref)}
  end

  def handle_cast(:browse, State[] = state) do
    {:ok, ref} = SD.browse("_genomu._tcp")
    {:noreply, state.browser(ref)}
  end

  def handle_info({:dnssd, browser, {:browse, :add, {description, service, domain}}},
                  State[browser: browser, instances: instances] = state) do
    {:ok, ref} = SD.resolve(description, service, domain)
    :ets.insert(instances, [{ref, {description, service, domain}}, {description, ref}])
    {:noreply, state}
  end

  def handle_info({:dnssd, browser, {:browse, :remove, {description, _service, _domain}}},
                  State[browser: browser, instances: instances] = state) do
    :ets.delete(instances, description)
    {:noreply, state}
  end

  def handle_info({:dnssd, ref, {:resolve, {_domain, _port, txt}}},
                  State[instances: instances] = state) do
    SD.stop(ref)
    [{_, {description, _service, _domain}}] = :ets.lookup(instances, ref)
    :ets.delete(instances, ref)
    if txt["cluster_name"] == Genomu.Cluster.name do
      :ets.insert(instances, {description, txt})
    end
    {:noreply, state}
  end

  def handle_info({:dnssd, _, {:register, :add, _}}, state) do
    {:noreply, state}
  end
  def handle_info({:dnssd, _, {:register, :remove, _}}, state) do
    {:noreply, state}
  end

  def terminate(_, State[registration: ref]) do
    SD.stop(ref)
  end

end