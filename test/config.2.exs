Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  config.instance_name (hostname |> to_binary) <> "2"
  config.http_port 9129
  config.handoff_port 8100
  config.quiet_mode true
end