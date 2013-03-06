defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0, name: :core

  @args 0
  def identity(value, _no_arg) do
    value
  end

  @args 1
  def set(_value, new_value) do
    new_value
  end

  @args 2
  def compose(value, MsgPack.fix_array(len: 2) = arr) do
    {[bin1, bin2], ""} = MsgPack.unpack(arr)
    {op1, rest} = Genomu.Operation.deserialize(bin1)
    {op2, rest} = Genomu.Operation.deserialize(bin2)
    value = Genomu.Operation.apply(op2, value)
    Genomu.Operation.apply(op1, value)
  end

end