Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.BinaryTest do
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

  test "append to nil", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, ["key"], API.Binary.append("123")) == "123"
  end

  test "append to binary", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity("1"))
    assert C.get(ch, ["key"], API.Binary.append("23")) == "123"
  end

  test "range", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity("Hello"))    
    assert C.get(ch, ["key"], API.Binary.range(1, 3)) == "ell"
  end

  test "size", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity("Hello"))    
    assert C.get(ch, ["unknown_key"], API.Binary.size) == 0
    assert C.get(ch, ["key"], API.Binary.size) == 5
  end

end