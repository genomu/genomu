Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.NumberTest do
  use Genomu.TestCase

  alias Genomu.Client, as: C
  alias Genomu.API, as: API

  setup_all do
    start_db
    {:ok, [conn: connection]}
  end

  teardown_all do
    stop_db
  end

  test "incr", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "key", API.Number.incr) == 1
    C.set(ch, "key", API.Core.identity(1))
    assert C.get(ch, "key", API.Number.incr) == 2
    assert C.get(ch, "key", API.Number.incr(10)) == 11
  end

  test "decr", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "key", API.Number.decr) == -1
    C.set(ch, "key", API.Core.identity(1))
    assert C.get(ch, "key", API.Number.decr) == 0
    assert C.get(ch, "key", API.Number.decr(10)) == -9
  end

end