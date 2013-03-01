defmodule Genomu.Cluster do
  
  def all_members do
    :riak_core_ring.all_members my_ring
  end

  def my_ring do
    {:ok, ring} = :riak_core_ring_manager.get_my_ring
    ring
  end

  def only_member? do
    all_members == [node]
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
    :riak_core.staged_join(node)
  end

end
