Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  config.instance_name (hostname |> to_binary) <> "1"
  config.hostname (hostname |> to_binary)
  config.http_port 9119
  config.handoff_port 8099
  config.quiet_mode true
  config.port 9101
end