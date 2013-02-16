defmodule Genomu.Utils do

  @doc """
  Returns current time (:erlang.now) in microseconds
  """
  @spec now_in_microseconds :: non_neg_integer

  def now_in_microseconds do
    {macro, sec, micro} = :erlang.now
    macro*1000000000000+sec*1000000+micro
  end

end