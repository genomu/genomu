defmodule Genomu.HTTP.Cluster do

  use Genomu.HTTP.Resource

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

  def to_json("/cluster", req, state) do
    json = [name: Genomu.Cluster.name,
            num_partitions: Genomu.Cluster.num_partitions,
    ]
    {json |> maybe_jsonp(req), req, state}
  end

  def to_json("/cluster/membership", req, state) do
    json = [instances: Enum.map(Genomu.Cluster.all_members_status,
                                fn({node, status}) ->
                                  [url: instance_url(node),
                                   node: node |> to_binary,
                                   status: status |> to_binary,
                                   indices: Genomu.Cluster.indices(node) |> Enum.map(&1 |> to_binary),
                                   future_indices: Genomu.Cluster.future_indices(node),
                                  ]
                                end),
            established: not Genomu.Cluster.only_member?]
    {json |> maybe_jsonp(req), req, state}
  end

  def to_json("/cluster/membership/staging", req, state) do
    case :riak_core_claimant.plan do
      {:error, :ring_not_ready} ->
        json = [status: "ring_not_ready", plan: plan([], [])]
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
    {json |> maybe_jsonp(req), req, state}
  end

  defp plan(changes, next_rings) do

    joins =
    Enum.filter_map(changes,
                fn({_, :join}) -> true
                  (_) ->  false
                end,
                fn({node, :join}) -> node end)
    leaves =
    Enum.filter_map(changes,
                fn({_, :leave}) -> true
                  (_) ->  false
                end,
                fn({node, :leave}) -> node end)

    forced_removals =
    Enum.filter_map(changes,
                fn({_, :force_remove}) -> true
                  (_) ->  false
                end,
                fn({node, :force_remove}) -> node end)

    replacements =
    Enum.filter_map(changes,
                fn({_, {:replace, _}}) -> true
                  (_) ->  false
                end,
                fn({node, {:replace, new_node}}) -> [{node, new_node}] end)

    forced_replacements =
    Enum.filter_map(changes,
                fn({_, {:force_replace, _}}) -> true
                  (_) ->  false
                end,
                fn({node, {:force_replace, new_node}}) -> [{node, new_node}] end)


    [transitions: length(next_rings),
     joins: Enum.map(joins, instance_url(&1)),
     leaves: Enum.map(leaves, instance_url(&1)),
     forced_removals: Enum.map(forced_removals, instance_url(&1)),
     replacements: Enum.map(replacements, instance_url(&1)),
     forced_replacements: Enum.map(forced_replacements, instance_url(&1))]
  end

  defp instance_url(node) do
     :rpc.call(node, Genomu, :instance_url, [])
  end

end