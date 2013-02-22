Genomu.Config.config do
  {:ok, hostname} = :inet.gethostname
  config.instance_name (hostname |> to_binary)
end