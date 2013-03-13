defmodule Genomu.Operation do
  @typep operation      :: atom
  @typep argument       :: binary
  @type  t              :: {module, operation, argument}
  @type  serialized     :: MsgPack.packed

  @spec next(binary) :: {binary, binary}
  def next(binary) do
    {module_id, binary} = MsgPack.next(binary)
    {operation_id, binary} = MsgPack.next(binary)
    {argument, binary} = MsgPack.next(binary)
    {module_id <> operation_id <> argument, binary}
  end

  @spec serialize(t) :: serialized
  def serialize({module, operation, argument}) do
    module_id = Genomu.Module.id(module)
    operation_id = Genomu.Module.operation(module, {:name, operation})[:id]
    [MsgPack.pack(module_id),
     MsgPack.pack(operation_id),
     argument] |> iolist_to_binary
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


  @modules [Genomu.Module.Core, Genomu.Module.Binary,
            Genomu.Module.List, Genomu.Module.Dict, Genomu.Module.Boolean,
            Genomu.Module.Number]

  lc module inlist @modules do
    Module.eval_quoted __MODULE__, (quote do: require unquote(module))
  end

  def modules, do: @modules

  lc module inlist @modules, {operation, _attrs} inlist Genomu.Module.operations(module) do
    module_id = Genomu.Module.id(module)
    operation_id = Genomu.Module.operation(module, {:name, operation})[:id]
    binary = binary_to_list(MsgPack.pack(module_id) <> MsgPack.pack(operation_id))
    def apply(<< unquote_splicing(binary), argument :: binary>>, value) do
      unquote(module).unquote(operation)(value, argument)
    end
  end

  lc module inlist @modules, {operation, _attrs} inlist Genomu.Module.operations(module) do
    module_id = Genomu.Module.id(module)
    operation_id = Genomu.Module.operation(module, {:name, operation})[:id]
    binary = binary_to_list(MsgPack.pack(module_id) <> MsgPack.pack(operation_id))
    def argument(<< unquote_splicing(binary), argument :: binary>>) do
      argument
    end
  end

  lc module inlist @modules, {operation, _attrs} inlist Genomu.Module.operations(module) do
    module_id = Genomu.Module.id(module)
    operation_id = Genomu.Module.operation(module, {:name, operation})[:id]
    binary = MsgPack.pack(module_id) <> MsgPack.pack(operation_id)
    def operation(<< unquote_splicing(binary_to_list(binary)), _ :: binary >>) do
      unquote(binary)
    end
  end


end