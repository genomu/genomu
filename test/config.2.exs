Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  config.instance_name (hostname |> to_binary) <> "2"
  config.hostname (hostname |> to_binary)
  config.http_port 9129
  config.handoff_port 8100
  config.quiet_mode true
  config.port 9102
end