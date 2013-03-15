defmodule Genomu.Module.List do
  use Genomu.Module, id: 2, name: :list

  @empty_value MsgPack.pack([])
  @true_value MsgPack.pack(true)
  @false_value MsgPack.pack(false)

  @args 0
  def head(MsgPack.fix_array(len: 0, rest: _rest), _no_arg) do
    @empty_value
  end
  def head(MsgPack.fix_array(rest: rest), _no_arg) do
    {head, _tail} = MsgPack.next(rest)
    head
  end
  def head(MsgPack.array16(len: 0, rest: _rest), _no_arg) do
    @empty_value
  end
  def head(MsgPack.array16(rest: rest), _no_arg) do
    {head, _tail} = MsgPack.next(rest)
    head
  end
  def head(MsgPack.array32(len: 0, rest: _rest), _no_arg) do
    @empty_value
  end
  def head(MsgPack.array32(rest: rest), _no_arg) do
    {head, _tail} = MsgPack.next(rest)
    head
  end

  @args 0
  def tail(MsgPack.fix_array(len: 0, rest: _rest), _no_arg) do
    @empty_value
  end
  def tail(MsgPack.fix_array(len: len, rest: rest), _no_arg) do
    {_, tail} = MsgPack.next(rest)
    MsgPack.fix_array(len: len - 1, rest: tail)
  end
  def tail(MsgPack.array16(len: 16, rest: rest), _no_arg) do
    {_, tail} = MsgPack.next(rest)
    MsgPack.fix_array(len: 15, rest: tail)
  end
  def tail(MsgPack.array16(len: len, rest: rest), _no_arg) do
    {_, tail} = MsgPack.next(rest)
    MsgPack.array16(len: len - 1, rest: tail)
  end
  def tail(MsgPack.array32(len: 0x10000, rest: rest), _no_arg) do
    {_, tail} = MsgPack.next(rest)
    MsgPack.array16(len: 0x10000 - 1, rest: tail)
  end
  def tail(MsgPack.array32(len: len, rest: rest), _no_arg) do
    {_, tail} = MsgPack.next(rest)
    MsgPack.array32(len: len - 1, rest: tail)
  end

  @args 1
  def append(MsgPack.fix_array(len: 15, rest: rest), element) do
    MsgPack.array16(len: 16, rest: rest <> element)
  end
  def append(MsgPack.fix_array(len: len, rest: rest), element) do
    MsgPack.fix_array(len: len + 1, rest: rest <> element)
  end
  def append(MsgPack.array16(len: 0x10000, rest: rest), element) do
    MsgPack.array32(len: 0x10000 + 1, rest: rest <> element)
  end
  def append(MsgPack.array16(len: len, rest: rest), element) do
    MsgPack.array16(len: len + 1, rest: rest <> element)
  end
  def append(MsgPack.array32(len: len, rest: rest), element) do
    MsgPack.array16(len: len + 1, rest: rest <> element)
  end
  def append(MsgPack.atom_nil, element) do
    MsgPack.fix_array(len: 1, rest: element)
  end

  @args 1
  def prepend(MsgPack.fix_array(len: 15, rest: rest), element) do
    MsgPack.array16(len: 16, rest: element <> rest)
  end
  def prepend(MsgPack.fix_array(len: len, rest: rest), element) do
    MsgPack.fix_array(len: len + 1, rest: element <> rest)
  end
  def prepend(MsgPack.array16(len: 0x10000, rest: rest), element) do
    MsgPack.array32(len: 0x10000 + 1, rest: element <> rest)
  end
  def prepend(MsgPack.array16(len: len, rest: rest), element) do
    MsgPack.array16(len: len + 1, rest: element <> rest)
  end
  def prepend(MsgPack.array32(len: len, rest: rest), element) do
    MsgPack.array16(len: len + 1, rest: element <> rest)
  end
  def prepend(MsgPack.atom_nil, element) do
    MsgPack.fix_array(len: 1, rest: element)
  end

  @args 0
  def length(MsgPack.fix_array(len: len), _no_arg) do
    MsgPack.pack(len)
  end

  def length(MsgPack.array16(len: len), _no_arg) do
    MsgPack.pack(len)
  end

  def length(MsgPack.array32(len: len), _no_arg) do
    MsgPack.pack(len)
  end

  def length(MsgPack.atom_nil(len: len), _no_arg) do
    MsgPack.pack(0)
  end

  @args 1
  def map(MsgPack.fix_array(len: len, rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    MsgPack.fix_array(len: len, rest: map_(rest, op, ""))
  end
  def map(MsgPack.array16(len: len, rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    MsgPack.array16(len: len, rest: map_(rest, op, ""))
  end
  def map(MsgPack.array32(len: len, rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    MsgPack.array32(len: len, rest: map_(rest, op, ""))
  end

  defp map_("", _, acc), do: acc
  defp map_(bin, op, acc) do
    {value, rest} = MsgPack.next(bin)
    result = Genomu.Operation.apply(op, value)
    map_(rest, op, acc <> result)
  end

  @args 1
  def filter(MsgPack.fix_array(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    {len, filtered} = filter_(rest, op, 0, "")
    MsgPack.fix_array(len: len, rest: filtered)
  end
  def filter(MsgPack.array16(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    {len, filtered} = filter_(rest, op, 0, "")
    MsgPack.array16(len: len, rest: filtered)
  end
  def filter(MsgPack.array32(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    {len, filtered} = filter_(rest, op, 0, "")
    MsgPack.array32(len: len, rest: filtered)
  end

  defp filter_("", _, len, acc), do: {len, acc}
  defp filter_(bin, op, len, acc) do
    {value, rest} = MsgPack.next(bin)
    case Genomu.Operation.apply(op, value) do
      MsgPack.atom_true ->
        filter_(rest, op, len + 1, acc <> value)
      MsgPack.atom_false ->
        filter_(rest, op, len, acc)
    end
  end

  @args 1
  def reduce(MsgPack.fix_array(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    acc0 = Genomu.Operation.argument(op)
    op0 = Genomu.Operation.operation(op)
    reduce_(rest, op0, acc0)
  end
  def reduce(MsgPack.array16(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    acc0 = Genomu.Operation.argument(op)
    op0 = Genomu.Operation.operation(op)
    reduce_(rest, op0, acc0)
  end
  def reduce(MsgPack.array32(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    acc0 = Genomu.Operation.argument(op)
    op0 = Genomu.Operation.operation(op)
    reduce_(rest, op0, acc0)
  end

  defp reduce_("", _, acc), do: acc
  defp reduce_(bin, op, acc) do
    {value, rest} = MsgPack.next(bin)
    new_value = Genomu.Operation.apply(op <> acc, value)
    reduce_(rest, op, new_value)
  end

  @args 1
  def any(MsgPack.fix_array(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    any_(rest, op)
  end
  def any(MsgPack.array16(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    any_(rest, op)
  end
  def any(MsgPack.array32(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    any_(rest, op)
  end

  defp any_("", _), do: @false_value
  defp any_(bin, op) do
    {value, rest} = MsgPack.next(bin)
    case Genomu.Operation.apply(op, value) do
      @true_value -> @true_value
      @false_value -> 
        any_(rest, op)
    end
  end

  @args 1
  def member(MsgPack.fix_array(rest: rest), v) do
    member_(rest, v)
  end
  def member(MsgPack.array16(rest: rest), v) do
    member_(rest, v)
  end
  def member(MsgPack.array32(rest: rest), v) do
    member_(rest, v)
  end

  defp member_("", _), do: @false_value
  defp member_(bin, v) do
    {value, rest} = MsgPack.next(bin)
    case value do
      ^v -> @true_value
      _ -> 
        member_(rest, v)
    end
  end

  @args 1
  def all(MsgPack.fix_array(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    all_(rest, op)
  end
  def all(MsgPack.array16(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    all_(rest, op)
  end
  def all(MsgPack.array32(rest: rest), op) do
    {op, ""} = MsgPack.unpack(op)
    all_(rest, op)
  end

  defp all_("", _), do: @true_value
  defp all_(bin, op) do
    {value, rest} = MsgPack.next(bin)
    case Genomu.Operation.apply(op, value) do
      @true_value -> all_(rest, op)
      @false_value -> @false_value
    end
  end

end