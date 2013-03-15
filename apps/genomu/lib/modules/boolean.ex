defmodule Genomu.Module.Boolean do
  use Genomu.Module, id: 4, name: :boolean

  @true_value MsgPack.pack(true)
  @false_value MsgPack.pack(false)

  @args 1
  def equals(value, value), do: @true_value
  def equals(_v1, _v2), do: @false_value

  @args 0
  @name :not
  def bool_not(@true_value, _no_arg), do: @false_value
  def bool_not(@false_value, _no_arg), do: @true_value

  @args 1
  @name :and
  def bool_and(@true_value, @true_value), do: @true_value
  def bool_and(@true_value, @false_value), do: @false_value
  def bool_and(@false_value, _), do: @false_value

  @args 1
  @name :or
  def bool_or(@true_value, _), do: @true_value
  def bool_or(_, @true_value), do: @true_value
  def bool_or(_, _), do: @false_value

end