defmodule Genomu.HTTP.Instance do

  use Genomu.HTTP.Resource

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def to_json(req, state) do
    json = [name: Genomu.instance_name,
            system_version: Genomu.system_version,
            instance_url: Genomu.instance_url,
            partitions: Genomu.Cluster.indices(Node.self) |> Enum.map(&1 |> to_binary),
            node: node |> to_binary]
    {json |> maybe_jsonp(req), req, state}
  end

end