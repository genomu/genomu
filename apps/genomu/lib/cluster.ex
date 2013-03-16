defmodule Genomu.Cluster do

  def all_members do
    :riak_core_ring.all_members(my_ring)
  end

  def all_members_status do
    :riak_core_ring.all_member_status(my_ring)
  end

  def my_ring do
    {:ok, ring} = :riak_core_ring_manager.get_my_ring
    ring
  end

  def indices(node) do
    :riak_core_ring.indices(my_ring, node)
  end

  def future_indices(node) do
    :riak_core_ring.future_indices(my_ring, node)
  end

  def only_member? do
    all_members == [node]
  end

  def num_partitions do
    :riak_core_ring.num_partitions(my_ring)
  end

  def name do
    Application.environment(:riak_core)[:cluster_name] |> to_binary
  end

  def join(url) when is_binary(url) do
    {:ok, 200, _headers, client} =
      :hackney.request(:get, url <> "/instance", 
                      [{"accept","application/json"}],"",[])
    {:ok, body, client} = :hackney.body(client)
    :hackney.close(client)
    json = :jsx.to_term(body)
    node = binary_to_atom(json["node"])
    join(node)
  end

  def join(node) when is_atom(node) do
    case :riak_core.staged_join(node) do
      :ok ->
        :ok = Genomu.Channel.sync_with(Genomu.Channel.Root, {Genomu.Channel.Root, node})
        :ok
      {:error, _} = error ->
        error
    end
  end

end
