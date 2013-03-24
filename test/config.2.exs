Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  user = System.get_env("USER")
  config.instance_name (hostname |> to_binary) <> "-#{user}2"
  config.hostname (hostname |> to_binary)
  config.http_port 9129
  config.handoff_port 8100
  config.quiet_mode true
  config.port 9102
end