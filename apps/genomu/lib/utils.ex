defmodule Genomu.Utils do

  @doc """
  Returns current time (:erlang.now) in microseconds
  """
  @spec now_in_microseconds :: non_neg_integer

  def now_in_microseconds do
    {macro, sec, micro} = :erlang.now
    macro*1000000000000+sec*1000000+micro
  end

  @spec host_id :: binary
  def host_id do
    :mochiglobal.get(Genomu.Utils.HostID)
  end

 
  def pad_bitstring(bitstring, pad) do
    padding = pad - rem(bit_size(bitstring),pad)
    if padding == pad do
      bitstring
    else 
      << bitstring :: bitstring, 0 :: size(padding) >>
    end
  end

end