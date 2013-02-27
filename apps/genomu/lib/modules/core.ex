defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0, name: :core

  @args 0
  def identity(value, _no_arg) do
    value
  end

  @args 1
  def set(_value, new_value) do
    new_value
  end

end