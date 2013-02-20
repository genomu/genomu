defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0

  @id 0
  def identity(value, _no_arg) do
    value
  end

  @id 1
  def set(_value, new_value) do
    new_value
  end

end