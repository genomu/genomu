ExUnit.start

defmodule Genomu.TestCase do
  use ExUnit.CaseTemplate

  using(_) do
    quote do
      import Genomu.TestCase
    end
  end

  defmacro qc(do: body) do
    quote do
      f =
      fn() ->
        unquote(body)
      end
      assert Proper.quickcheck(f.(), numtests: 100) == true
    end
  end
end