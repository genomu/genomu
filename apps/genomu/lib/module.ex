defmodule Genomu.Module do
  defmacro __using__(opts) do
    if nil?(opts[:id]) do
      raise ArgumentError, message: "module id is required"
    end
    quote do
      require MsgPack
      Module.register_attribute __MODULE__, :module_id, accumulate: false
      Module.register_attribute __MODULE__, :operation
      @on_definition Genomu.Module
      @module_id unquote(opts[:id])

      defimpl MsgPack.Protocol, for: __MODULE__ do
        def pack(operation) do
          (Genomu.Operation.serialize(operation) |> MsgPack.packed_to_binary) |>
          MsgPack.pack |> MsgPack.packed_to_binary
        end
      end

    end
  end

  def id(module) do
    [id] = module.__info__(:attributes)[:module_id]
    id
  end

  def operation(module, operation) do
    attributes =
    lc {:operation, [{op, attrs}]} inlist module.__info__(:attributes),
       op == operation, do: attrs
    Enum.reduce(attributes, [], fn(attrs, acc) -> Keyword.merge(acc, attrs) end)
  end

  def __on_definition__(env, _kind, name, [_, _], _guards, _body) do
    module     = env.module
    operation_id = Module.get_attribute(module, :id)
    operation    = [id: operation_id, name: name]
    Module.put_attribute(module, :operation, {name, operation})
    Module.put_attribute(module, :operation, {{:id, operation_id}, operation})
    quoted = quote do
      def unquote(name)(data) do
        Genomu.Operation.new(__MODULE__, unquote(name),
                             MsgPack.pack(data) |> MsgPack.packed_to_binary)
      end
    end
    Module.eval_quoted module, quoted
  end
  def __on_definition__(_env, _kind, _name, [_], _guards, _body) do
  end
end