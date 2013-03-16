defmodule ITC do
  use Bitwise, only_operators: true

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

  defp norm_events({n, m, m}) when is_integer(m), do: n + m
  defp norm_events({n, e1, e2}) do
    m = minimum(minimum(e1), minimum(e2))
    {n + m, sink(e1, m), sink(e2, m)}
  end

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

  defp sink({n, e1, e2}, m) do
    {n - m, e1, e2}
  end
  defp sink(n, m), do: n - m

  defp join_events({n1, l1, r1}, {n2, l2, r2}) when n1 > n2 do
    join_events({n2, l2, r2}, {n1, l1, r1})
  end
  defp join_events({n1, l1, r1}, {n2, l2, r2}) do
    norm_events({n1, join_events(l1, lift(l2, n2-n1)), join_events(r1, lift(r2, n2-n1))})
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
    case fill(i, e) do
      ^e ->
        {e1, _} = grow(i, e)
        {i, e1}
      e1 ->
        {i, e1}
    end
  end

  defp fill(0, e), do: e
  defp fill(1, e), do: maximum(e)
  defp fill({1, ir},{n, el, er}) do
    er1 = fill(ir, er)
    norm_events({n, maximum(maximum(el), minimum(er1)), er1})
  end
  defp fill({il, 1},{n, el, er}) do
    el1 = fill(il, el)
    norm_events({n, el1, maximum(maximum(er), minimum(el1))})
  end
  defp fill({il, ir},{n, el, er}) do
    norm_events({n, fill(il, el), fill(ir, er)})
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

  @spec eq(t, t) :: boolean
  def eq(t, t), do: true
  def eq({_, n1}, {_, n2}), do: _eq(n1, n2)
  def _eq(t, t), do: true
  def _eq(_, _), do: false

  @spec leq(t, t) :: boolean
  def leq({_, n1}, {_, n2}), do: _leq(n1, n2)
  def _leq({n1, l1, r1}, {n2, l2, r2}) do
    n1 <= n2 and
    _leq(lift(l1, n1), lift(l2, n2)) and
    _leq(lift(r1, n1), lift(r2, n2))
  end
  def _leq(n1, {n2, _l2, _r2}) do
    n1 <= n2
  end
  def _leq({n1, l1, r1}, n2) do
    n1 <= n2 and
    _leq(lift(l1, n1), n2) and
    _leq(lift(r1, n1), n2)
  end
  def _leq(n1, n2), do: n1 <= n2

  @spec le(t, t) :: boolean
  def le(t1, t2) do
    not(eq(t1, t2)) and leq(t1, t2)
  end

  @spec concurrent?(t,t) :: boolean
  def concurrent?(t1, t2) do
    not(leq(t1, t2)) and not(leq(t2, t1))
  end

  @spec joinable?(t,t) :: boolean
  def joinable?(t1, t2) do
    try do
      sum(t1, t2)
      true
    rescue
      FunctionClauseError -> false
    end
  end

  @spec encode(t) :: binary
  def encode({i, e}), do: <<encode_id(i) :: bits, encode_event(e) :: bits>>

  defp encode_id(0), do: <<0 :: 2, 0 :: 1>>
  defp encode_id(1), do: <<0 :: 2, 1 :: 1>>
  defp encode_id({0, i}), do: <<1 :: 2, encode_id(i) :: bits>>
  defp encode_id({i, 0}), do: <<2 :: 2, encode_id(i) :: bits>>
  defp encode_id({l, r}) do
    <<3 :: 2, encode_id(l) :: bits, encode_id(r) :: bits>>
  end

  defp encode_event({0, 0, r}), do:
    <<0 :: 1, 0 :: 2, encode_event(r) :: bits>>
  defp encode_event({0, l, 0}), do:
    <<0 :: 1, 1 :: 2, encode_event(l) :: bits>>
  defp encode_event({0, l, r}), do:
    <<0 :: 1, 2 :: 2, encode_event(l) :: bits, encode_event(r) :: bits>>
  defp encode_event({n, 0, r}), do:
    <<0 :: 1, 3 :: 2, 0 :: 1, 0 :: 1,
       encode_event(n) :: bits, encode_event(r) :: bits>>
  defp encode_event({n, l, 0}), do:
    <<0 :: 1, 3 :: 2, 0 :: 1, 1 :: 1,
       encode_event(n) :: bits, encode_event(l) :: bits>>
  defp encode_event({n, l, r}), do:
    <<0 :: 1, 3 :: 2, 1 :: 1,
       encode_event(n) :: bits, encode_event(l) :: bits, encode_event(r) :: bits>>
  defp encode_event(n), do: encode_number(n, 2)

  defp encode_number(n, b) when n < (1 <<< b) do
    <<((1 <<< b) - 2) :: [size(b)], n :: [size(b)]>>
  end
  defp encode_number(n, b), do: encode_number(n - (1 <<< b), b + 1)

  @spec decode(binary) :: t
  def decode(b) do
    {i, b} = decode_interval(b)
    {e, _} = decode_event(b)
    {i, e}
  end

  defp decode_interval(<<0 :: 2, 0 :: 1, b :: bits>>), do: {0, b}
  defp decode_interval(<<0 :: 2, 1 :: 1, b :: bits>>), do: {1, b}
  defp decode_interval(<<1 :: 2, b :: bits>>) do
    {i, b} = decode_interval(b)
    {{0, i}, b}
  end
  defp decode_interval(<<2 :: 2, b :: bits>>) do
    {i, b} = decode_interval(b)
    {{i, 0}, b}
  end
  defp decode_interval(<<3 :: 2, b :: bits>>) do
    {l, b} = decode_interval(b)
    {r, b} = decode_interval(b)
    {{l, r}, b}
  end

  defp decode_event(<<0 :: 1, 0 :: 2, b :: bits>>) do
    {r, b} = decode_event(b)
    {{0, 0, r}, b}
  end
  defp decode_event(<<0 :: 1, 1 :: 2, b :: bits>>) do
    {l, b} = decode_event(b)
    {{0, l, 0}, b}
  end
  defp decode_event(<<0 :: 1, 2 :: 2, b :: bits>>) do
    {l, b} = decode_event(b)
    {r, b} = decode_event(b)
    {{0, l, r}, b}
  end
  defp decode_event(<<0 :: 1, 3 :: 2, 0 :: 1, 0 :: 1, b :: bits>>) do
    {n, b} = decode_event(b)
    {r, b} = decode_event(b)
    {{n, 0, r}, b}
  end
  defp decode_event(<<0 :: 1, 3 :: 2, 0 :: 1, 1 :: 1, b :: bits>>) do
    {n, b} = decode_event(b)
    {l, b} = decode_event(b)
    {{n, l, 0}, b}
  end
  defp decode_event(<<0 :: 1, 3 :: 2, 1 :: 1, b :: bits>>) do
    {n, b} = decode_event(b)
    {l, b} = decode_event(b)
    {r, b} = decode_event(b)
    {{n, l, r}, b}
  end
  defp decode_event(<<1 :: 1, b :: bits>>), do: decode_number(b, 2, 0)

  defp decode_number(<<0 :: 1, r :: bits>>, b, acc) do
    <<n :: [size(b)], r :: bits>> = r
    {n + acc, r}
  end
  defp decode_number(<<1 :: 1, r :: bits>>, b, acc) do
    decode_number(r, b + 1, acc + (1 <<< b))
  end

  @spec encode_binary(t) :: binary
  @spec encode_binary(t, pad :: pos_integer) :: binary
  def encode_binary(clock, pad // 8) do
    encoded = encode(clock)
    remainder = rem(bit_size(encoded), pad)
    if remainder == 0 do
      encoded
    else
      padding = pad - remainder
      << encoded :: bitstring, 0 :: size(padding) >>
    end
  end

  @spec encode_string(t) :: String.t
  def encode_string(t) do
    bc <<b>> inbits encode_binary(t), do: <<(integer_to_binary(b,16))::binary>>
  end

  @spec to_string(t) :: String.t
  def to_string({i,e}) do
    iolist_to_binary [to_string_i(i), to_string_e(e)]
  end

  @spec to_string_i(interval) :: iolist
  defp to_string_i(0), do: "0"
  defp to_string_i(1), do: ""
  defp to_string_i({0, i}), do: ["r", to_string_i(i)]
  defp to_string_i({i, 0}), do: ["l", to_string_i(i)]
  defp to_string_i({l,r}), do: ["(l", to_string_i(l), "+", "r", to_string_i(r),")"]

  @spec to_string_e(events) :: iolist
  defp to_string_e({n, l, 0}), do: [to_string_e(n), "l", to_string_e(l)]
  defp to_string_e({n, 0, r}), do: [to_string_e(n), "r", to_string_e(r)]
  defp to_string_e({n, l, r}), do: [to_string_e(n), "(l", to_string_e(l), "+r", to_string_e(r),")"]
  defp to_string_e(n) when n > 0, do: integer_to_binary(n)
  defp to_string_e(_), do: ""

end