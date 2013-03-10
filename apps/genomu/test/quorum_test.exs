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

    C.execute(conn, ch, n: 10, do: C.apply(ch, ["k"], API.Core.identity("123")))
    assert {:error, Genomu.Client.TimeoutException[]} = C.execute(conn, ch, n: 10, r: 11, do: C.apply(ch, ["k"], API.Core.identity))
  end

end
