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
    assert C.get(ch, ["key"], API.Core.identity) == nil
    assert C.set(ch, ["key"], API.Core.identity("123")) == "123"
    assert C.get(ch, ["key"], API.Core.identity) == "123"
  end

  test "compose", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    assert C.get(ch, ["key"], API.Core.compose(API.Core.identity, API.Core.identity("123"))) == "123"
  end

  test "assert", context do
    conn = context[:conn]

    C.execute conn, ch, do: C.set(ch, ["akey"], API.Core.identity("123"))
   
    {:ok, ch} = C.begin(conn)
    assert C.get(ch, ["akey"], API.Core.assert(API.Boolean.equals?("123"))) == "123"
    assert_raise Genomu.Client.AbortException, fn ->
      C.get(ch, ["akey"], API.Core.assert(API.Boolean.equals?("321")))
    end
    assert C.get(ch, ["akey"], API.Core.assert(API.Boolean.equals?("123"))) == "123"
  end

  test "assert in a transaction replay", context do
    conn = context[:conn]

    {:ok, ch} = C.begin(conn)
    {:ok, ch1} = C.begin(conn)

    C.apply(ch, "tkey", API.Core.assert(API.Boolean.equals?(nil)))
    C.apply(ch1, "tkey", API.Core.assert(API.Boolean.equals?(nil)))

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
      C.apply(ch, "lkey", API.Core.assert(API.Boolean.equals?(nil)))
    end

  end

end