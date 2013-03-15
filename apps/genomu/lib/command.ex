defrecord Genomu.Command, n: 3, r: 2, vnodes: :any,
                          cell: nil, type: nil, operation: nil,
                          new_revision: nil, timeout: 5000 do

  @type type :: :set | :apply | :get

  record_type n: pos_integer, r: pos_integer, vnodes: :any | :primary,
              cell: Genomu.cell, 
              type: type, operation: Genomu.Operation.serialized,
              new_revision: Genomu.revision, timeout: non_neg_integer

  @spec set(Keyword.t | Genomu.operation) :: t
  def set(options) when is_list(options) do
    new(Keyword.merge(options, type: :set))
  end
  def set(op), do: new(type: :set, operation: op)

  @spec get(Keyword.t | Genomu.operation) :: t
  def get(options) when is_list(options) do
    new(Keyword.merge(options, type: :get))
  end
  def get(op), do: new(type: :get, operation: op)

  @spec apply(Keyword.t | Genomu.operation) :: t
  def apply(options) when is_list(options) do
    new(Keyword.merge(options, type: :apply))
  end
  def apply(op), do: new(type: :apply, operation: op)

  core_module = binary_to_list(MsgPack.pack(Genomu.Module.id(Genomu.Module.Core)))
  assert_op = binary_to_list(MsgPack.pack(Genomu.Module.operation(Genomu.Module.Core, {:name, :assert})[:id]))

  @spec assertion?(t) :: boolean
  def assertion?(__MODULE__[operation: << unquote_splicing(core_module), unquote_splicing(assert_op), _ :: binary >>]) do
    true
  end
  def assertion?(__MODULE__[]), do: false

end