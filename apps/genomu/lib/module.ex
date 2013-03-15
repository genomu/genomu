defmodule Genomu.Module do
  defmacro __using__(opts) do
    if nil?(opts[:id]) do
      raise ArgumentError, message: "module id is required"
    end
    quote do
      require MsgPack
      Module.register_attribute __MODULE__, :module_id, accumulate: false
      Module.register_attribute __MODULE__, :module_name, accumulate: false
      Module.register_attribute __MODULE__, :operation
      @on_definition Genomu.Module
      @after_compile Genomu.Module
      @module_id unquote(opts[:id])
      @module_name unquote((opts[:name] || __MODULE__) |> to_binary)

      @id 0
    end
  end

  def id(module) do
    [id] = module.__info__(:attributes)[:module_id]
    id
  end

  def name(module) do
    [name] = module.__info__(:attributes)[:module_name]
    name
  end

  def operation(module, operation) do
    attributes =
    lc {:operation, [{op, attrs}]} inlist module.__info__(:attributes),
       op == operation, do: attrs
    Enum.reduce(attributes, [], fn(attrs, acc) -> Keyword.merge(acc, attrs) end)
  end

  def operations(module) do
    lc {:operation, [{{:name, op}, attrs}]} inlist module.__info__(:attributes), do: {op, attrs}
  end

  def __on_definition__(env, :def, name, [_, _, _], _guards, _body) do
    module = env.module
    last_operation = Module.get_attribute(module, :last_operation)
    operation_id = Module.get_attribute(module, :id)
    args = Module.get_attribute(module, :args)
    public_name = Module.get_attribute(module, :name) || name
    unless last_operation == {name, args} do
      operation = [id: operation_id, name: name, public_name: public_name, args: args]
      Module.put_attribute(module, :operation, {{:name, name}, operation})
      Module.put_attribute(module, :operation, {{:id, operation_id}, operation})
      quoted = quote do
        @id (@id + 1)
        @last_operation {unquote(name), unquote(args)}
        @name nil
      end
      Module.eval_quoted module, quoted
    end
  end
  def __on_definition__(_env, _kind, _name, _, _guards, _body) do
  end

  def __after_compile__(Macro.Env[] = env, _binary) do
    module = env.module
    json = to_json(module) |> :jsx.to_json(indent: 2)
    dir = Path.expand("../../priv/modules", __FILE__)
    filename = Path.join(dir,"#{name(module)}.json")
    File.mkdir_p(dir)
    File.write(filename, json)
  end

  def to_json(module, version // Genomu.Mixfile.version) do
    case module.__info__(:moduledoc) do
      {_, module_doc} when is_binary(module_doc) -> :ok
      _ -> module_doc = :null
    end
    [
      system_version: version,
      id: id(module),
      name: name(module),
      doc: module_doc,
      operations:
        (lc {_, attrs} inlist operations(module) do
          name = attrs[:public_name] |> to_binary
          {_, _, _, _, doc} = List.keyfind(module.__info__(:docs), {attrs[:name], 3}, 0)
          doc = doc || :null
          [id: attrs[:id], name: name, args: attrs[:args], doc: doc]
         end),
    ]
  end
end