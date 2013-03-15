Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.DictTest do
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

  test "value", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Dict.value("k", 1))
    C.set(ch, "key", API.Dict.value("k1", 1))
    C.set(ch, "key", API.Dict.value("k", 2))
    assert C.get(ch, "key", API.Dict.value("k")) == 2
  end

  test "keys", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Dict.value("k", 1))
    C.set(ch, "key", API.Dict.value("k1", 1))
    C.set(ch, "key", API.Dict.value("k", 2))
    assert Enum.sort(C.get(ch, "key", API.Dict.keys)) == 
           Enum.sort(["k","k1"])
  end

  test "values", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    C.set(ch, "key", API.Dict.value("k", 1))
    C.set(ch, "key", API.Dict.value("k1", 1))
    C.set(ch, "key", API.Dict.value("k", 2))
    assert Enum.sort(C.get(ch, "key", API.Dict.values)) == 
           Enum.sort([1, 2])
  end

  test "update", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    dict = MsgPack.Map.from_list([{"ctr", 0}])
    C.set(ch, "key", API.Core.identity(MsgPack.Map.from_list([{"ctr", 0}, {"dict", dict}])))
    assert C.set(ch, "key", API.Dict.update("ctr", API.Number.incr)) ==
           MsgPack.Map.from_list([{"ctr", 1},{"dict", dict}])
    assert C.set(ch, "key", API.Dict.update("dict",API.Dict.update("ctr", API.Number.incr))) ==
           MsgPack.Map.from_list([{"ctr", 1},{"dict",MsgPack.Map.from_list([{"ctr", 1}])}])
  end

end