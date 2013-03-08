defmodule Genomu.Module.Number do
  use Genomu.Module, id: 5, name: :number

  @nil_value MsgPack.pack(nil)
  @one_value MsgPack.pack(1)
  @minus_one_value MsgPack.pack(-1)

  @args 0
  @name :incr
  def incr(@nil_value, _no_arg), do: @one_value
  def incr(value, _no_arg) do
    MsgPack.pack(elem(MsgPack.unpack(value), 0) + 1)
  end

  @args 1
  @name :incr
  def incr_by(@nil_value, incr), do: incr
  def incr_by(value, incr) do
    MsgPack.pack(elem(MsgPack.unpack(value), 0) + elem(MsgPack.unpack(incr), 0))
  end

  @args 0
  @name :decr
  def decr(@nil_value, _no_arg), do: @minus_one_value
  def decr(value, _no_arg) do
    MsgPack.pack(elem(MsgPack.unpack(value), 0) - 1)
  end

  @args 1
  @name :decr
  def decr_by(@nil_value, decr), do: MsgPack.pack(- elem(MsgPack.unpack(decr), 0))
  def decr_by(value, decr) do
    MsgPack.pack(elem(MsgPack.unpack(value), 0) - elem(MsgPack.unpack(decr), 0))
  end

end