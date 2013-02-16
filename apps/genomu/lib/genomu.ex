defmodule Genomu do
  @moduledoc """
  Basic Genomu API module. It is recommended to `use Genomu`
  since it will require Genomu modules containing common macros
  """

  defmacro __using__(_) do
    quote do
      require Genomu.Operation
    end
  end

  ##
  ## Core data types
  ##
  @type key       :: [binary, ...]
  @type operation :: Genomu.Operation.t
  @type timestamp :: non_neg_integer

  @type revision
  @type cell      :: {key, revision}

  @doc """
  Starts the application
  """
  def start do
    :ok = Application.start(:genomu)
  end

  @type command   :: {:set, operation} |
                     {:apply, operation} |
                     {:get, operation}

end
