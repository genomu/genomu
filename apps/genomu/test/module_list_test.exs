Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.ListTest do
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

  test "head", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3]))
    assert C.get(ch, ["key"], API.List.head) == 1
    C.set(ch, ["key"], API.Core.identity([]))
    assert C.get(ch, ["key"], API.List.head) == []
  end

  test "tail", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3]))
    assert C.get(ch, ["key"], API.List.tail) == [2,3]
    C.set(ch, ["key"], API.Core.identity([]))
    assert C.get(ch, ["key"], API.List.tail) == []
  end

  test "append", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, ["key"], API.List.append(1)) == [1]
    assert C.set(ch, ["key"], API.List.append(2)) == [1,2]
  end

  test "prepend", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, ["key"], API.List.prepend(1)) == [1]
    assert C.set(ch, ["key"], API.List.prepend(2)) == [2,1]
  end

  test "length", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, ["key"], API.List.length) == 0
    C.set(ch, ["key"], API.List.prepend(1))
    assert C.get(ch, ["key"], API.List.length) == 1
  end

  test "map", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3,"a"]))
    assert C.get(ch, ["key"], API.List.map(API.Boolean.equals?(2))) == [false, true, false, false]
  end

  test "reduce", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3,4,5]))
    assert C.get(ch, ["key"], API.List.reduce(API.Number.incr(1))) == 16
  end

  test "any?", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3,4,5]))
    assert C.get(ch, ["key"], API.List.any?(API.Boolean.equals?(2))) == true
    assert C.get(ch, ["key"], API.List.any?(API.Boolean.equals?(100))) == false
  end

  test "member?", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,2,3,4,5]))
    assert C.get(ch, ["key"], API.List.member?(3)) == true
    assert C.get(ch, ["key"], API.List.member?(6)) == false
  end


  test "all?", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, ["key"], API.Core.identity([1,1,1]))
    assert C.get(ch, ["key"], API.List.all?(API.Boolean.equals?(1))) == true
    C.set(ch, ["key"], API.Core.identity([1,1,2]))
    assert C.get(ch, ["key"], API.List.all?(API.Boolean.equals?(1))) == false
  end

end