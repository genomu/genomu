Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.BasicTest do
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

  test "channel retrieving a key that has no value", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "unset", API.Core.identity) == nil
  end

  test "channel retrieving a key that was committed by another channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "unused", API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "unused", API.Core.identity) == "123"
  end

  test "channel retrieving a key that was set by the same channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "another", API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "another", API.Core.identity("321")) == "321"
    assert C.get(ch, "another", API.Core.identity) == "321"
  end

  test "concurrently committing channels", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    C.set(ch, "ctr", API.Number.incr)
    C.set(ch, "ctr", API.Number.incr)
    C.set(ch1, "ctr", API.Number.incr)

    C.commit(ch1)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "ctr", API.Core.identity) == 3
  end

  test "channel retrieving a key at a specified version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn} = C.set(ch, "some", API.Core.identity("123"), vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, {"some", vsn}, API.Core.identity, vsn: true) == {"123", vsn}
  end

  test "channel retrieving a key at a specified upper bound version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn0} = C.set(ch, "some1", API.Core.identity("123"), vsn: true)
    {"123", vsn} = C.set(ch, "some2", API.Core.identity("123"), vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, {"some1", vsn}, API.Core.identity, vsn: true, txn: true) == {"123", vsn0, vsn}
  end

  test "channel retrieving an operation at an inexisting address", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.operation(ch, "noop") == nil
  end

  test "channel retrieving an operation that was committed by another channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "someop", API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.operation(ch, "someop") == {API.Core, :identity, ["123"]}
  end

  test "channel retrieving an operation that was set by the same channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "anotherop", API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "anotherop", API.Core.identity("321")) == "321"
    assert C.operation(ch, "anotherop") == {API.Core, :identity, ["321"]}
  end

  test "retrieving operation from concurrently committing channels", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    C.set(ch, "opctr", API.Number.incr)
    C.set(ch, "opctr", API.Number.incr)
    C.set(ch1, "opctr", API.Number.incr)

    C.commit(ch1)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.operation(ch, "opctr") == {API.Number, :incr, []}
  end

  test "channel retrieving an operation at a specified version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn} = C.set(ch, "someop1", API.Core.identity("123"), vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.operation(ch, {"someop1", vsn}, vsn: true) == {{API.Core, :identity, ["123"]}, vsn}
  end

  test "channel retrieving an operation at a specified upper bound version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn0} = C.set(ch, "someop1x", API.Core.identity("123"), vsn: true)
    {"123", vsn} = C.set(ch, "someop2x", API.Core.identity("123"), vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert C.operation(ch, {"someop1x", vsn}, vsn: true, txn: true) == {{API.Core, :identity, ["123"]}, vsn0, vsn}
  end

end
