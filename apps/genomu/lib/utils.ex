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
    {:ok, ifaces} = :inet.getifaddrs
    Enum.find_value(ifaces, fn({_iface, opts}) ->
                              if opts[:hwaddr] do
                                list_to_binary(opts[:hwaddr])
                              end
                            end)
  end


end