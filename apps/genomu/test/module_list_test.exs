Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.ListTest do
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

  test "head", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Core.identity([1,2,3]))
    assert Ch.get(ch, ["key"], API.List.head) == 1
    Ch.set(ch, ["key"], API.Core.identity([]))
    assert Ch.get(ch, ["key"], API.List.head) == []
  end

  test "tail", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Core.identity([1,2,3]))
    assert Ch.get(ch, ["key"], API.List.tail) == [2,3]
    Ch.set(ch, ["key"], API.Core.identity([]))
    assert Ch.get(ch, ["key"], API.List.tail) == []
  end

  test "append", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.set(ch, ["key"], API.List.append(1)) == [1]
    assert Ch.set(ch, ["key"], API.List.append(2)) == [1,2]
  end

  test "prepend", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.set(ch, ["key"], API.List.prepend(1)) == [1]
    assert Ch.set(ch, ["key"], API.List.prepend(2)) == [2,1]
  end

  test "length", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, ["key"], API.List.length) == 0
    Ch.set(ch, ["key"], API.List.prepend(1))
    assert Ch.get(ch, ["key"], API.List.length) == 1
  end

  test "map", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Core.identity([1,2,3,"a"]))
    assert Ch.get(ch, ["key"], API.List.map(API.Boolean.equals?(2))) == [false, true, false, false]
  end

  test "reduce", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Core.identity([1,2,3,4,5]))
    assert Ch.get(ch, ["key"], API.List.reduce(API.Number.incr(1))) == 16
  end

  test "any?", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Core.identity([1,2,3,4,5]))
    assert Ch.get(ch, ["key"], API.List.any?(API.Boolean.equals?(2))) == true
    assert Ch.get(ch, ["key"], API.List.any?(API.Boolean.equals?(100))) == false
  end


end