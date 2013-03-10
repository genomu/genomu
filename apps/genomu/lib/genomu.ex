defmodule Genomu do

  ##
  ## Core data types
  ##
  @type key       :: [binary, ...]
  @type operation :: Genomu.Operation.t
  @type timestamp :: non_neg_integer

  @type revision  :: binary
  @type cell      :: {key, revision}

  ## Internal types
  @type gen_server_ref  :: pid | atom | {atom, node} | {:global, atom} |
                           {:via, module, atom}

  @doc """
  Starts the application
  """
  def start do
    :ok = Application.start(:genomu)
  end

  def system_version do
    {:ok, keys} = :application.get_all_key(:genomu)
    keys[:vsn] |> to_binary
  end

  def instance_url do
    env = Application.environment(:genomu)
    hostname = env[:hostname] || "localhost"
    port = env[:http_port] || 9119
    "http://#{hostname}:#{port}"
  end

  def instance_name do
    Application.environment(:genomu)[:instance_name] 
  end

  @type command   :: Genomu.Command.t

end
