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
    {path, req} = :cowboy_req.path(req)
    create_path(path, req, state)  
  end

  def create_path("/cluster/membership", req, state) do
      {"/cluster/membership/staging", req, state}
  end
  def create_path("/cluster/membership/staging", req, state) do
      {"/cluster/membership", req, state}
  end

  def create_membership(req, state) do
    {path, req} = :cowboy_req.path(req)
    create_membership(path, req, state)
  end

  def create_membership("/cluster/membership", req, state) do
    {:ok, body, req} = :cowboy_req.body(req)
    json = :jsx.to_term(body)
    url = json["instance_url"]
    Genomu.Cluster.join(url)
    {true, req, state}
  end

  def create_membership("/cluster/membership/staging", req, state) do
    case :riak_core_claimant.commit do
      :ok -> {true, req, state}
      _ -> {false, req, state} # TODO: improve
    end
  end  

  def to_json(req, state) do
    {path, req} = :cowboy_req.path(req)
    to_json(path, req, state)
  end

  def to_json("/cluster/membership", req, state) do
    json = [instances: Enum.map(Genomu.Cluster.all_members,
                       fn(node) ->
                         :rpc.call(node, Genomu, :instance_url, [])
                       end),
            established: not Genomu.Cluster.only_member?]
    {json |> :jsx.to_json, req, state}
  end

  def to_json("/cluster/membership/staging", req, state) do
    case :riak_core_claimant.plan do
      {:error, :ring_not_ready} ->
        json = [status: "ring_not_ready", plan: [{}]]
      {:ok, changes, next_rings} ->
        # From riak_core:
        #   The last next ring is always the final ring after all changes,
        #   which is uninteresting to show. Only print N-1 rings.
        case next_rings do
          [] -> :ok
          [_|_] ->
            next_rings = next_rings |> Enum.reverse |> tl |> Enum.reverse
        end
        json = [status: "ready", plan: plan(changes, next_rings)]
    end
    {json |> :jsx.to_json, req, state}
  end

  defp plan(changes, next_rings) do

    joins =
    Enum.filter_map(changes,
                fn({_, :join}) -> true
                  (_) ->  false
                end,
                fn({node, :join}) -> node |> to_binary end)
    leaves =
    Enum.filter_map(changes,
                fn({_, :leave}) -> true
                  (_) ->  false
                end,
                fn({node, :leave}) -> node |> to_binary end)

    forced_removals =
    Enum.filter_map(changes,
                fn({_, :force_remove}) -> true
                  (_) ->  false
                end,
                fn({node, :force_remove}) -> node |> to_binary end)

    replacements =
    Enum.filter_map(changes,
                fn({_, {:replace, _}}) -> true
                  (_) ->  false
                end,
                fn({node, {:replace, new_node}}) -> [{node |> to_binary, new_node |> to_binary}] end)

    forced_replacements =
    Enum.filter_map(changes,
                fn({_, {:force_replace, _}}) -> true
                  (_) ->  false
                end,
                fn({node, {:force_replace, new_node}}) -> [{node |> to_binary, new_node |> to_binary}] end)


    [transitions: length(next_rings),
     joins: joins,
     leaves: leaves,
     forced_removals: forced_removals,
     replacements: replacements,
     forced_replacements: forced_replacements]
  end

end