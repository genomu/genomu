defmodule Genomu.HTTP.Cluster do

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def allowed_methods(req, state) do
    {["GET", "POST"], req, state}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def content_types_accepted(req, state) do
    {[{{"application","json", []}, :create_membership}], req, state}
  end

  def post_is_create(req, state) do
    {true, req, state}
  end

  def create_path(req, state) do
    {"/cluster/membership", req, state}
  end

  def create_membership(req, state) do
    {:ok, body, req} = :cowboy_req.body(req)
    json = :jsx.to_term(body)
    url = json["instance_url"]
    Genomu.Cluster.join(url)
    {true, req, state}
  end


  def to_json(req, state) do
    json = [nodes: Enum.map(Genomu.Cluster.all_members, to_binary(&1)),
            established: not Genomu.Cluster.only_member?]
    {json |> :jsx.to_json, req, state}
  end

end