defmodule Genomu.Operation do
  @typep operation      :: atom
  @typep argument       :: binary
  @type  t              :: {module, operation, argument}
  @type  serialized     :: MsgPack.packed

  @spec apply(t, value :: term) :: term
  def apply({module, operation, argument}, value) do
    :erlang.apply(module, operation, [value, argument])
  end

  @spec serialize(t) :: serialized
  def serialize({module, operation, argument}) do
    module_id = Genomu.Module.id(module)
    operation_id = Genomu.Module.operation(module, {:name, operation})[:id]
    [MsgPack.pack(module_id),
     MsgPack.pack(operation_id),
     MsgPack.pack(argument)] |> iolist_to_binary
  end

  @spec deserialize(binary) :: {t, binary}
  def deserialize(bin) do
      {module_id, rest} = MsgPack.unpack(bin)
      {operation_id, rest} = MsgPack.unpack(rest)
      {argument, rest} = MsgPack.next(rest)
      module = Genomu.Commands.module(module_id)
      operation = Genomu.Module.operation(module, {:id, operation_id})[:name]
      {{module, operation, argument}, rest}
  end

  @doc """
  A helper function that creates a Operation.t structure
  """
  @spec new(module, operation, argument) :: t
  def new(module, operation, argument) do
    {module, operation, MsgPack.pack(argument)}
  end

  @doc """
  A macro that allows creating a Operation.t in a call-like syntax:

      Genomu.Operation.new Genomu.Module.Core.set("123") #=>
      {Genomu.Module.Core,:set,<<163,49,50,51>>}

  This macro is only intended to be used for debugging purposes
  """
  defmacro new({{:., _, [module, operation]}, _, [argument]}) do
    quote do
      Genomu.Operation.new(unquote(module), unquote(operation), unquote(argument))
    end
  end

end