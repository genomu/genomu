defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0, name: :core

  @false_value MsgPack.pack(false)

  @args 0
  def identity(value, _no_arg, _opts) do
    value
  end

  @args 1
  @name :identity
  def set_identity(_value, new_value, _opts) do
    new_value
  end

  @args 2
  def compose(value, MsgPack.fix_array(len: 2) = arr, opts) do
    {[bin1, bin2], ""} = MsgPack.unpack(arr)
    {op1, ""} = Genomu.Operation.next(bin1)
    {op2, ""} = Genomu.Operation.next(bin2)
    value = Genomu.Operation.apply(op2, value, opts)
    Genomu.Operation.apply(op1, value, opts)
  end

  @args 1
  def assert(value, op, opts) do
    {op, ""} = MsgPack.unpack(op)
    if Genomu.Operation.apply(op, value, opts) == @false_value do
      raise Genomu.Operation.AbortException
    end
    value
  end

  @args 0
  def version(_, _, opts) do
    MsgPack.pack(opts[:version])
  end

  @args 0
  def operation(_, _, opts) do
    {{module, operation, arg}, _} = Genomu.Operation.deserialize(opts[:operation])
    {arg, _} = MsgPack.unpack(arg)
    MsgPack.pack(MsgPack.Map.from_list([{"module", Genomu.Module.id(module)},
                                        {"operation", Genomu.Module.operation(module, {:name, operation})[:id]},
                                        {"argument", arg}
                                       ]))
  end

end