defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0

  @id 0
  def identity(value, _no_arg) do
    value
  end

  @id 1
  def set(_value, new_value) do
    new_value
  end

  @id 2
  def identified(value, MsgPack.fix_array(len: 2, rest: rest)) do
    {_id, rest} = MsgPack.next(rest)
    {command, ""} = MsgPack.unpack(rest)
    {cmd, ""} = Genomu.Operation.deserialize(command)
    Genomu.Operation.apply(cmd, value)
  end

end