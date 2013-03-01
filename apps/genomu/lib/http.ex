defmodule Genomu.HTTP do

  alias :cowboy, as: Cowboy

  def start do
    env = Application.environment(:genomu)
    http_port = env[:http_port] || 9119

    dispatch = [
      {:_, [
        {"/instance", Genomu.HTTP.Instance, []},
        {"/cluster/membership", Genomu.HTTP.Cluster, []},
      ]},
    ] |> :cowboy_router.compile

    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: http_port],
                                  [env: [dispatch: dispatch]])
  end
end