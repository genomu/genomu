defmodule Genomu.HTTP.Page do

  def init(_transport, req, []) do
    {:ok, req, nil}
  end

  def handle(req, state) do
    {path, req} = :cowboy_req.path(req)
    handle_path(path, req, state)
  end

  def handle_path("/template/dashboard", req, state) do
    page("Dashboard", dashboard_page([]), req, state)
  end
  def handle_path("/template/instances", req, state) do
    page("Instances", instances_page([]), req, state)
  end
  def handle_path("/template/partitions", req, state) do
    page("Partitions", partitions_page([]), req, state)
  end
  def handle_path(_, req, state) do
    {:ok, req} = :cowboy_req.reply(200, [{"content-type","text/html"}],
                                   page([]), req)
    {:ok, req, state}
  end

  defp page(name, template, req, state) do
    {:ok, req} = :cowboy_req.reply(200, [{"content-type","text/html"}],
                                 view_page(view: template, 
                                           pages: pages(name)), req)
    {:ok, req, state}
  end

  def terminate(_reason, _req, _state), do: :ok

  require EEx
  EEx.function_from_file :defp, :page,
                         Path.expand("../templates/index.html.eex", __FILE__),
                         [:_assigns]
  EEx.function_from_file :defp, :view_page,
                         Path.expand("../templates/view.html.eex", __FILE__),
                         [:assigns]
  EEx.function_from_file :defp, :dashboard_page,
                         Path.expand("../templates/dashboard.html.eex", __FILE__),
                         [:_assigns]
  EEx.function_from_file :defp, :instances_page,
                         Path.expand("../templates/instances.html.eex", __FILE__),
                         [:_assigns]
  EEx.function_from_file :defp, :partitions_page,
                         Path.expand("../templates/partitions.html.eex", __FILE__),
                         [:_assigns]

  defp pages(active) do
    [
     {"Dashboard", "/#/", "bar-chart"},
     {"Instances", "/#/instances", "hdd"},
     {"Partitions", "/#/partitions", "cogs"},
    ] |> Enum.map(fn({name, url, icon}) -> {name, url, icon, name == active} end)
  end
end