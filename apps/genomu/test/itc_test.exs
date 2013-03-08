defmodule Genomu.ITC.Test do
  use Genomu.TestCase

  import ExUnit.Assertions
  use Proper.Properties
  alias :proper_types, as: Types

  defp clock() do
    {t0, _} = ITC.fork(ITC.seed)
    clock(t0)
  end
  defp clock(clock) do
    {t1, _} = ITC.fork(clock)
    sized(size, clock(size, clock, t1))
  end
  defp clock(0, t0, t1) do
    frequency([
      {1, {t0, t1}},
      {4, {t0, ITC.event(t0)}},
     ])
  end
  defp clock(size, t0, t1) do
    frequency([
      {1, clock(0, t0, t1)},
      {3, clock(div(size, 2), t0, ITC.event(t0))},
      {3, lazy(
        letshrink res = clock(div(size, 2), t1, elem(ITC.fork(t1), 0)), do: res
        )},
      {3, lazy(
        letshrink res = clock(div(size, 2), t1, elem(ITC.fork(t1), 1)), do: res
        )},
     ])
  end

  defp concurrent do
    (let {{_, t1}, {_, t2}} = (let {_, t0} = clock do
      {t1, t2} = ITC.fork(t0)
      {clock(t1), clock(t2)}
     end), do: {ITC.event(t1), ITC.event(t2)})
  end
  defp different do
    (let {t1, t2} =
       (let {t1, t2} = clock, when: not(ITC.leq(t1, t2) and ITC.leq(t2, t1))
       ), do: {ITC.event(t1), ITC.event(t2)})
  end
  defp equal do
    frequency([
      {1, (let {t0, t1} = clock, when: ITC.leq(t0, t1) and ITC.leq(t1, t0))},
      {9, (let {_, t0} = clock, do: {t0, equal(t0)})},
     ])
  end
  defp equal(t0) do
    sized(size, equal(size, t0))
  end
  defp equal(0, t0), do: t0
  defp equal(size, t0) do
    frequency([
      {1, t0},
      {4, lazy(
        letshrink res = equal(div(size, 2), elem(ITC.fork(t0), 0)), do: res
        )},
      {5, lazy(
        letshrink res = equal(div(size, 2), elem(ITC.fork(t0), 1)), do: res
        )},
     ])
  end

  test "seed/seed\?" do
    qc do
      forall itc in Types.frequency([{9, Types.term}, {1, ITC.seed}]) do
        is_boolean(ITC.seed?(itc))
      end
    end
  end

  test "fork" do
    qc do
      forall {_, t0} in clock do
        {t1, t2} = ITC.fork t0
        t1 != t0 and t2 != t0
          and ITC.leq(t0, t1)
          and ITC.leq(t0, t2)
          and ITC.leq(t0, ITC.join(t1, t2))
      end
    end
  end

  test "join siblings" do
    qc do
      forall {_, t0} in clock do
        {t1, t2} = ITC.fork t0
        res = ITC.join t1, t2
        t1 != res and t2 != res
           and ITC.leq(t0, res)
           and ITC.leq(t1, res) and ITC.leq(t2, res)
      end
    end
  end

  test "join concurrent" do
    qc do 
      forall {t1, t2} in concurrent do
        res = ITC.join t1, t2
        t1 != res and t2 != res
           and ITC.leq(t1, res) and ITC.leq(t2, res)
           and size(res) <= size(t1)
           and size(res) <= size(t2)
      end
    end
  end

  test "event" do
    qc do
      forall {_, t0} in clock do
        res = ITC.event t0
        res != t0 and ITC.leq(t0, res)
      end
    end
  end

  test "leq when t0 < t1" do
    qc do
      forall {t0, t1} in different do
        ITC.leq(t0, t1) and not(ITC.le(t1, t0)) and not(ITC.eq(t0, t1))
      end
    end
  end

  test "leq when t0 > t1" do
    qc do
      forall {t0, t1} in different do
        if ITC.eq(t0, t1), do: t1 = ITC.event t1
        not(ITC.leq(t1, t0)) and ITC.le(t0, t1) and not(ITC.eq(t0, t1))
      end
    end
  end

  test "leq when t0 == t1" do
    qc do
      forall {t0, t1} in equal do
        ITC.leq(t0, t1) and ITC.leq(t1, t0) and ITC.eq(t0, t1)
      end
    end
  end

  test "leq when t0 and t1 are concurrent" do
    qc do
      forall {t1, t2} in concurrent do
        ITC.concurrent?(t1, t2) and not(ITC.leq(t1, t2)) and not(ITC.leq(t2, t1))
      end
    end
  end

  test "le when t0 < t1" do
    qc do
      forall {t0, t1} in different do
        ITC.le(t0, t1) and not(ITC.le(t1, t0)) and not(ITC.eq(t0, t1))
      end
    end
  end

  test "le when t0 > t1" do
    qc do
      forall {t0, t1} in different do
        not(ITC.le(t1, t0)) and ITC.le(t0, t1) and not(ITC.eq(t0, t1))
      end
    end
  end

  test "le when t0 == t1" do
    qc do
      forall {t0, t1} in equal do
        not(ITC.le(t1, t0)) and ITC.eq(t0, t1)
      end
    end
  end

  test "le when t0 and t1 are concurrent" do
    qc do
      forall {t1, t2} in concurrent do
        ITC.concurrent?(t1, t2) and not(ITC.le(t1, t2)) and not(ITC.le(t2, t1))
      end
    end
  end

  test "eq when t0 == t1" do
    qc do
      forall {t0, t1} in equal do
        ITC.eq(t0, t1) and ITC.eq(t1, t0)
      end
    end
  end

  test "eq when t0 != t1" do
    qc do
      forall {t0, t1} in different do
        not(ITC.eq(t0, t1)) and not(ITC.eq(t1, t0))
          and (ITC.le(t0, t1) or ITC.le(t1, t0))
      end
    end
  end

  test "concurrent\?" do
    qc do
      forall {t0, t1} in clock do
        ITC.concurrent?(t0, t1) == (not(ITC.leq(t0, t1)) and not(ITC.leq(t1, t0)))
      end
    end
  end

  test "encode/decode" do
    qc do
      forall {_, t} in clock do
        ITC.decode(ITC.encode(t)) == t
      end
    end
  end

  test "encode_binary/decode" do
    qc do
      forall {_, t} in clock do
        ITC.decode(ITC.encode_binary(t)) == t
      end
    end
  end

end
