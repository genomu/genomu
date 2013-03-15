Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.Module.CoreTest do
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

  test "identity", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "key", API.Core.identity) == nil
    assert C.set(ch, "key", API.Core.identity("123")) == "123"
    assert C.get(ch, "key", API.Core.identity) == "123"
  end

  test "compose", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "key", API.Core.compose(API.Core.identity, API.Core.identity("123"))) == "123"
  end

  test "assert", context do
    conn = context[:conn]

    C.execute conn, ch, do: C.set(ch, "akey", API.Core.identity("123"))
   
    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "akey", API.Core.assert(API.Boolean.equals("123"))) == "123"
    assert_raise Genomu.Client.AbortException, fn ->
      C.get(ch, "akey", API.Core.assert(API.Boolean.equals("321")))
    end
    assert C.get(ch, "akey", API.Core.assert(API.Boolean.equals("123"))) == "123"
  end

  test "assert in a transaction replay", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    C.apply(ch, "tkey", API.Core.assert(API.Boolean.equals(nil)))
    C.apply(ch1, "tkey", API.Core.assert(API.Boolean.equals(nil)))

    C.set(ch, "tkey", API.Core.identity("ch"))
    C.set(ch1, "tkey", API.Core.identity("ch1"))

    C.commit(ch1)

    assert_raise Genomu.Client.AbortException, fn ->
      C.commit(ch)
    end

    assert (C.execute conn, ch, do: C.get(ch, "tkey", API.Core.identity)) == "ch1"
  end

  test "late assertion", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)

    C.set(ch, "lkey", API.Core.identity("ch"))
  
    assert_raise Genomu.Client.AbortException, fn ->
      C.apply(ch, "lkey", API.Core.assert(API.Boolean.equals(nil)))
    end

  end

  test "retrieving an operation at an inexisting address", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "noop", API.Core.operation) == nil
  end

  defp encode_op(MsgPack.Map[map: map]) do
    MsgPack.pack(map["module"]) <> MsgPack.pack(map["operation"]) <> MsgPack.pack(map["argument"])
  end

  test "retrieving an operation that was committed by another channel", context do
    conn = context[:conn]

    op = API.Core.identity("123")

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "someop", op) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert encode_op(C.get(ch, "someop", API.Core.operation)) == op
  end

  test "retrieving an operation that was set by the same channel", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.set(ch, "anotherop", API.Core.identity("123")) == "123"
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    op = API.Core.identity("321")
    assert C.set(ch, "anotherop", op) == "321"
    assert encode_op(C.get(ch, "anotherop", API.Core.operation)) == op
  end

  test "retrieving operation from concurrently committing channels", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    op = API.Number.incr
    C.set(ch, "opctr", op)
    C.set(ch, "opctr", op)
    C.set(ch1, "opctr", op)

    C.commit(ch1)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    assert encode_op(C.get(ch, "opctr", API.Core.operation)) == op
  end

  test "retrieving an operation at a specified version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    op = API.Core.identity("123")
    {"123", vsn} = C.set(ch, "someop1", op, vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    {result, vsn1} = C.get(ch, {"someop1", vsn}, API.Core.operation, vsn: true)
    assert encode_op(result) == op
    assert vsn1 == vsn
  end

  test "retrieving an operation at a specified upper bound version", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    op = API.Core.identity("123")
    {"123", vsn0} = C.set(ch, "someop1x", op, vsn: true)
    {"123", vsn} = C.set(ch, "someop2x", op, vsn: true)
    C.commit(ch)

    {:ok, ch} = C.begin(conn)
    {result, vsn1, txn1} = C.get(ch, {"someop1x", vsn}, API.Core.operation, vsn: true, txn: true)
    assert encode_op(result) == op
    assert vsn1 == vsn0
    assert txn1 == vsn
  end

  test "retrieving version for an inexisting object", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, "something", API.Core.version) == ""
  end

  test "retrieving version for an existing object", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {"123", vsn} = C.set(ch, "something", API.Core.identity("123"), vsn: true)
    assert C.get(ch, "something", API.Core.version) == vsn
  end
end