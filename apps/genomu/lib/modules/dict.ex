defmodule Genomu.Module.Dict do
  use Genomu.Module, id: 3, name: :dict

  @nil_value MsgPack.pack(nil)

  @args 1
  def value(MsgPack.fix_map(len: 0), _key) do
    @nil_value
  end
  def value(MsgPack.fix_map(rest: rest), key) do
    value_(rest, key)
  end
  def value(MsgPack.map16(rest: rest), key) do
    value_(rest, key)
  end
  def value(MsgPack.map32(rest: rest), key) do
    value_(rest, key)
  end

  defp value_("", _key), do: @nil_value
  defp value_(bin, key) do
    sz = byte_size(key)
    case bin do
      << ^key :: [binary, size(sz)], rest :: binary >> ->
        {next, _} = MsgPack.next(rest)      
        next
      _ ->
       {_key, bin} = MsgPack.next(bin)
       {_value, bin} = MsgPack.next(bin)
       value_(bin, key)
    end
  end

  # TODO: find a better name than "set"
  @args 2
  def set(MsgPack.fix_map(len: 15, rest: rest), MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.map16(len: 16, rest: rest <> pair)
  end
  def set(MsgPack.fix_map(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.fix_map(len: len + 1, rest: rest <> pair)
  end
  def set(MsgPack.map16(len: 0x10000, rest: rest), MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.map32(len: 0x10000 + 1, rest: rest <> pair)
  end
  def set(MsgPack.map16(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.map16(len: len + 1, rest: rest <> pair)
  end
  def set(MsgPack.map32(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.map32(len: len + 1, rest: rest <> pair)
  end
  def set(@nil_value, MsgPack.fix_array(len: 2, rest: pair)) do
    MsgPack.fix_map(len: 1, rest: pair)
  end

end