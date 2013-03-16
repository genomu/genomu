defrecord Genomu.Command, n: 3, r: 2, vnodes: :any,
                          cell: nil, type: nil, operation: nil,
                          new_revision: nil, timeout: 5000 do

  @type type :: :set | :apply | :get

  record_type n: pos_integer, r: pos_integer, vnodes: :any | :primary,
              cell: Genomu.cell, 
              type: type, operation: Genomu.Operation.serialized,
              new_revision: Genomu.revision, timeout: non_neg_integer

  core_module = binary_to_list(MsgPack.pack(Genomu.Module.id(Genomu.Module.Core)))
  assert_op = binary_to_list(MsgPack.pack(Genomu.Module.operation(Genomu.Module.Core, {:name, :assert})[:id]))

  @spec assertion?(t) :: boolean
  def assertion?(__MODULE__[operation: << unquote_splicing(core_module), unquote_splicing(assert_op), _ :: binary >>]) do
    true
  end
  def assertion?(__MODULE__[]), do: false

end