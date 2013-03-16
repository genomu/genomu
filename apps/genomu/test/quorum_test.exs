Code.require_file "../test_helper.exs", __FILE__

defmodule Genomu.QuorumTest do
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

  test "high read quorum results in a timeout", context do
    conn = context[:conn]

    C.execute(conn, ch, n: 10, do: C.apply(ch, "k", API.Core.identity("123")))
    assert_raise Genomu.Client.TimeoutException, fn ->
      C.execute(conn, ch, n: 10, r: 11, timeout: 4000, do: C.apply(ch, "k", API.Core.identity))
    end
  end

  test "failing quorum results in a timeout", context do
    conn = context[:conn]

    [_,_,{last, _}] = :riak_core_apl.get_apl(:crypto.sha(["k1"]), 3, :genomu)

    breaker_pid = vnode_breaker(last, fn(_pid, _msg) ->  end)

    assert_raise Genomu.Client.TimeoutException, fn ->
      C.execute(conn, ch, n: 3, r: 3, timeout: 4000, do: C.apply(ch, "k1", API.Core.identity("123")))
    end

    unbreak_vnodes
    Process.exit(breaker_pid, :normal)
  end

end
