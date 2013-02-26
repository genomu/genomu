defrecord Genomu.Command, n: 3, r: 2, vnodes: :any,
                          cell: nil, type: nil, operation: nil,
                          new_revision: nil do

  @type type :: :set | :apply | :get

  record_type n: pos_integer, r: pos_integer, vnodes: :any | :primary,
              cell: Genomu.cell, 
              type: type, operation: Genomu.operation,
              new_revision: Genomu.revision

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

end