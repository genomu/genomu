defmodule ITC do
  @typep interval :: term
  @typep events :: term
  @opaque t :: {interval, events}

  @spec seed :: t
  def seed, do: {1,0}

  @spec seed?(t) :: boolean
  def seed?(i), do: i == seed

  @spec fork(t) :: {t,t}
  def fork({i, e}) do
    {i1, i2} = split(i)
    {{i1, e}, {i2, e}}
  end

  defp split(0), do: {0,0}
  defp split(1), do: {{1,0},{0,1}}
  defp split({0,i}) do
    {i1, i2} = split(i)
    {{0,i1}, {0,i2}}
  end
  defp split({i, 0}) do
    {i1, i2} = split(i)
    {{i1, 0}, {i2, 0}}
  end
  defp split({i1, i2}) do
    {{i1, 0}, {0, i2}}
  end

  @spec join(t,t) :: t
  def join({i1,e1},{i2,e2}) do
    {sum(i1, i2), join_events(e1, e2)}
  end

  defp sum(0, i), do: i
  defp sum(i, 0), do: i
  defp sum({l1, r1},{l2,r2}) do
    norm({sum(l1, l2), sum(r1, r2)})
  end

  defp norm({0,0}), do: 0
  defp norm({1,1}), do: 1
  defp norm(i), do: i

  defp minimum({n, e1, e2}) do
    n + minimum(minimum(e1), minimum(e2))
  end
  defp minimum(n), do: n

  defp minimum(n1, n2) do
    min(minimum(n1), minimum(n2))
  end

  defp maximum({n, e1, e2}) do
    n + maximum(maximum(e1), maximum(e2))
  end
  defp maximum(n), do: n

  defp maximum(n1, n2) do
    max(maximum(n1), maximum(n2))
  end

  defp lift({n, e1, e2}, m) do
    {n + m, e1, e2}
  end
  defp lift(n, m), do: n + m

  #defp sink({n, e1, e2}, m) do
  #  {n - m, e1, e2}
  #end
  #defp sink(n, m), do: n - m

  defp join_events({n1, l1, r1}, {n2, l2, r2}) when n1 > n2 do
    join_events({n2, l2, r2}, {n1, l1, r1})
  end
  defp join_events({n1, l1, r1}, {n2, l2, r2}) do
    norm({n1, join_events(l1, lift(l2, n2-n1)), join_events(r1, lift(r2, n2-n1))})
  end
  defp join_events(n1, {n2, l2, r2}) do
    join_events({n1, 0, 0}, {n2, l2, r2})
  end
  defp join_events({n1, l1, r1}, n2) do
    join_events({n1, l1, r1}, {n2, 0, 0})
  end
  defp join_events(n1, n2), do: maximum(n1, n2)

  @spec event(t) :: t
  def event({i,e}) do
    if fill(i,e) != e do
      {i, fill(i, e)}
    else
      {e1, _} = grow(i, e)
      {i, e1}
    end
  end

  defp fill(0, e), do: e
  defp fill(1, e), do: maximum(e)
  defp fill({1, ir},{n, el, er}) do
    er1 = fill(ir, er)
    norm({n, maximum(maximum(el), minimum(er1)), er1})
  end
  defp fill({il, 1},{n, el, er}) do
    el1 = fill(il, el)
    norm({n, el1, maximum(maximum(er), minimum(el1))})
  end
  defp fill({il, ir},{n, el, er}) do
    norm({n, fill(il, el), fill(ir, er)})
  end
  defp fill(_i, n), do: n

  defp grow(1, n), do: {n+1, 0}
  defp grow({0, ir}, {n, el, er}) do
    {er1, cr} = grow(ir, er)
    {{n, el, er1}, cr + 1}
  end
  defp grow({il, 0}, {n, el, er}) do
    {el1, cl} = grow(il, el)
    {{n, el1, er}, cl + 1}
  end
  defp grow({il, ir}, {n, el, er}) do
    {el1, cl} = grow(il, el)
    {er1, cr} = grow(ir, er)
    if cl < cr do
      {{n, el1, er}, cl + 1}
    else
      {{n, el, er1}, cr + 1}
    end
  end
  defp grow(i, n) do
    {e1, c} = grow(i, {n, 0, 0})
    {e1, c + 1000}
  end

  @spec leq(t, t) :: boolean
  def leq({_, n1}, {_, n2}), do: _leq(n1, n2)
  def _leq({n1, l1, r1}, {n2, l2, r2}) do
    n1 <= n2 and
    _leq(lift(l1, n1), lift(l2, n2)) and
    _leq(lift(r1, n1), lift(r2, n2))
  end
  def _leq(n1, {n2, _l2, _r2}) when is_integer(n1) do
    n1 <= n2
  end
  def _leq({n1, l1, r1}, n2) when is_integer(n2) do
    n1 <= n2 and
    _leq(lift(l1, n1), n2) and
    _leq(lift(r1, n1), n2)
  end
  def _leq(n1, n2) when is_integer(n1) and is_integer(n2), do: n1 <= n2
  def _leq(t, t), do: true

end