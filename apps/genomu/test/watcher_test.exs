Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.WatcherTest do
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

  test "notifications for single-key transactions", context do
    conn = context[:conn]

    me = self
    {:ok, _w, ref} = C.watch(conn, fn(ref, k, co) -> me <- {ref, k, co} end, [["mykey"]])

    C.execute conn, ch, do: C.set(ch, ["mykey"], API.Core.identity(1))

    assert_receive {^ref, ["mykey"], _}, 2_000
  end

  test "notifications for multi-key transactions", context do
    conn = context[:conn]

    me = self
    {:ok, _w, ref} = C.watch(conn, fn(ref, k, co) -> me <- {ref, k, co} end, [["mykey1"],["mykey2"]])

    C.execute conn, ch do
      C.set(ch, ["mykey1"], API.Core.identity(1))
      C.set(ch, ["mykey2"], API.Core.identity(1))
    end

    assert_receive {^ref, ["mykey1"], _}, 2_000
    assert_receive {^ref, ["mykey2"], _}, 2_000
  end

end
