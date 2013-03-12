defprotocol Genomu.Storage do
  @type t

  @spec init(t, term) :: {:ok, t} | {:error, reason :: term}
  def init(t, options)

  @spec lookup(t, Genomu.cell) :: {value :: binary, version :: Genomu.revision, txn :: Genomu.revision}
  def lookup(t, cell)

  @spec stage(t, Genomu.cell, operation :: Genomu.Operation.serialized, value :: binary) :: :ok
  def stage(t, cell, operation, value)

  @spec commit(t, Genomu.revision, entries :: [Genomu.cell], log :: [Genomu.cell], commit_object :: binary) :: :ok
  def commit(t, revision, entries, log, commit_object)

  @spec delete(t) :: :ok
  def delete(t)

  @spec size(t) :: non_neg_integer
  def size(t)

  @spec reduce(t, acc0 :: term,
               ((key :: term, value :: term, acc0 :: term) -> acc :: term)) :: term
  def reduce(t, acc0, f)

  @spec unpack(t, term) :: :ok
  def unpack(t, data)

  @spec close(t) :: :ok
  def close(t)
end