Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  config.instance_name (hostname |> to_binary) <> "3"
  config.http_port 9139
  config.handoff_port 8101
  config.quiet_mode true
end