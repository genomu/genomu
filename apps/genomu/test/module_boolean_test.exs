Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.BooleanTest do
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

  test "equals", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Core.identity("123"))
    assert C.get(ch, "key", API.Boolean.equals("123"))
  end

  test "not", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Core.identity(true))
    assert C.get(ch, "key", API.Boolean.not) == false
    C.set(ch, "key", API.Core.identity(false))
    assert C.get(ch, "key", API.Boolean.not) == true
  end

  test "and", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Core.identity(true))
    assert C.get(ch, "key", API.Boolean.and(true)) == true
    assert C.get(ch, "key", API.Boolean.and(false)) == false
    C.set(ch, "key", API.Core.identity(false))
    assert C.get(ch, "key", API.Boolean.and(true)) == false
    assert C.get(ch, "key", API.Boolean.and(false)) == false
  end

  test "or", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Core.identity(true))
    assert C.get(ch, "key", API.Boolean.or(true)) == true
    assert C.get(ch, "key", API.Boolean.or(false)) == true
    C.set(ch, "key", API.Core.identity(false))
    assert C.get(ch, "key", API.Boolean.or(true)) == true
    assert C.get(ch, "key", API.Boolean.or(false)) == false
  end

end