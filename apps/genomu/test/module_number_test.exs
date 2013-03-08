Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.NumberTest do
  use Genomu.TestCase

  alias Genomu.Client, as: C
  alias Genomu.Client.Channel, as: Ch
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
    assert Ch.get(ch, ["key"], API.Number.incr) == 1
    Ch.set(ch, ["key"], API.Core.identity(1))
    assert Ch.get(ch, ["key"], API.Number.incr) == 2
    assert Ch.get(ch, ["key"], API.Number.incr(10)) == 11
  end

  test "decr", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, ["key"], API.Number.decr) == -1
    Ch.set(ch, ["key"], API.Core.identity(1))
    assert Ch.get(ch, ["key"], API.Number.decr) == 0
    assert Ch.get(ch, ["key"], API.Number.decr(10)) == -9
  end

end