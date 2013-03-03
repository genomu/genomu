defmodule Genomu.HTTP do

  alias :cowboy, as: Cowboy

  def start do
    env = Application.environment(:genomu)
    http_port = env[:http_port] || 9119

    dispatch = [
      {:_, [
        {"/instance", Genomu.HTTP.Instance, []},
        {"/cluster/[membership[/staging]]", Genomu.HTTP.Cluster, []},
      ]},
    ] |> :cowboy_router.compile

    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: http_port],
                                  [env: [dispatch: dispatch]])
  end

  defp static do
    [directory: {:priv_dir, :genomu, ["static"]},
     mimetypes: {function(:mimetypes.path_to_mimes/2), :default}
    ]
  end
end