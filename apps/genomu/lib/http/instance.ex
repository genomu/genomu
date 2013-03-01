defmodule Genomu.HTTP.Instance do

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def to_json(req, state) do
    json = [root_channel: Genomu.Channel.clock(Genomu.Channel.Root) |> ITC.to_string,
            system_version: Genomu.system_version,
            node: node |> to_binary]
    {json |> :jsx.to_json, req, state}
  end

end