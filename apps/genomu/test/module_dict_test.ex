Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.DictTest do
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

  test "value", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Dict.value("k", 1))
    Ch.set(ch, ["key"], API.Dict.value("k1", 1))
    Ch.set(ch, ["key"], API.Dict.value("k", 2))
    assert Ch.get(ch, ["key"], API.Dict.value("k")) == 2
  end

  test "keys", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Dict.value("k", 1))
    Ch.set(ch, ["key"], API.Dict.value("k1", 1))
    Ch.set(ch, ["key"], API.Dict.value("k", 2))
    assert Enum.sort(Ch.get(ch, ["key"], API.Dict.keys)) == 
           Enum.sort(["k","k1","k"])
  end

  test "values", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    Ch.set(ch, ["key"], API.Dict.value("k", 1))
    Ch.set(ch, ["key"], API.Dict.value("k1", 1))
    Ch.set(ch, ["key"], API.Dict.value("k", 2))
    assert Enum.sort(Ch.get(ch, ["key"], API.Dict.values)) == 
           Enum.sort([1, 1, 2])
  end

end