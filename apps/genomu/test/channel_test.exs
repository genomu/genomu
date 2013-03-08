Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.ChannelTest do
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

  test "channel retrieving a key that has no value", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, ["unset"], API.Core.identity) == nil
  end

  test "channel retrieving a key that was committed by another channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.set(ch, ["unused"], API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, ["unused"], API.Core.identity) == "123"
  end

  test "channel retrieving a key that was set by the same channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert Ch.set(ch, ["another"], API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert Ch.set(ch, ["another"], API.Core.identity("321")) == "321"
    assert Ch.get(ch, ["another"], API.Core.identity) == "321"
  end

  test "concurrently committing channels", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    Ch.set(ch, ["ctr"], API.Number.incr)
    Ch.set(ch, ["ctr"], API.Number.incr)
    Ch.set(ch1, ["ctr"], API.Number.incr)

    C.commit(ch1)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, ["ctr"], API.Core.identity) == 3
  end

  test "channel retrieving a key at a specified version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn} = Ch.set(ch, ["some"], API.Core.identity("123"), version: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, {["some"], vsn}, API.Core.identity, version: true) == {"123", vsn}
  end

  test "channel retrieving a key at a specified upper bound version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn0} = Ch.set(ch, ["some1"], API.Core.identity("123"), version: true)
    {"123", vsn} = Ch.set(ch, ["some2"], API.Core.identity("123"), version: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert Ch.get(ch, {["some1"], vsn}, API.Core.identity, version: true, txn: true) == {"123", vsn0, vsn}
  end

end
