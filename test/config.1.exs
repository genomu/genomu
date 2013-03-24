Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  user = System.get_env("USER")
  config.instance_name (hostname |> to_binary) <> "#{user}1"
  config.hostname (hostname |> to_binary)
  config.http_port 9119
  config.handoff_port 8099
  config.quiet_mode true
  config.port 9101
end