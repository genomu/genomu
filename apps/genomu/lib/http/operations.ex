defmodule Genomu.HTTP.Operations do

  use Genomu.HTTP.Resource

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def allowed_methods(req, state) do
    {["GET"], req, state}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end


  def to_json(req, state) do
    {path, req} = :cowboy_req.path(req)
    to_json(path, req, state)
  end

  def to_json("/operations", req, state) do
    json = lc module inlist Genomu.Operation.modules do
      details = Genomu.Module.to_json(module, Genomu.system_version)
      {details[:name], details}
    end
    {json |> maybe_jsonp(req), req, state}
  end

end