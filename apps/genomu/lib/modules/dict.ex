defmodule Genomu.Module.Dict do
  use Genomu.Module, id: 3, name: :dict

  @nil_value MsgPack.pack(nil)

  @args 1
  def value(MsgPack.fix_map(len: 0), _key, _opts) do
    @nil_value
  end
  def value(MsgPack.fix_map(rest: rest), key, _opts) do
    value_(rest, key)
  end
  def value(MsgPack.map16(rest: rest), key, _opts) do
    value_(rest, key)
  end
  def value(MsgPack.map32(rest: rest), key, _opts) do
    value_(rest, key)
  end

  defp value_("", _key), do: @nil_value
  defp value_(bin, key) do
    sz = byte_size(key)
    case bin do
      << ^key :: [binary, size(sz)], rest :: binary >> ->
        {next, _rest} = MsgPack.next(rest)      
        next
      _ ->
       {_key, bin} = MsgPack.next(bin)
       {_value, bin} = MsgPack.next(bin)
       value_(bin, key)
    end
  end

  @args 2
  @name :value 
  def set(MsgPack.fix_map(len: 15, rest: rest), MsgPack.fix_array(len: 2, rest: pair), _opts) do
    {key, new_val} = MsgPack.next(pair)
    {new, appendp} = set_(rest, key, new_val)
    if appendp do
      MsgPack.map16(len: 16, rest: new)
    else
      MsgPack.fix_map(len: 15, rest: new)
    end
  end
  def set(MsgPack.fix_map(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), _opts) do
    {key, new_val} = MsgPack.next(pair)
    {new, appendp} = set_(rest, key, new_val)
    if appendp do
      MsgPack.fix_map(len: len + 1, rest: new)
    else
      MsgPack.fix_map(len: len, rest: new)
    end
  end
  def set(MsgPack.map16(len: 0x10000, rest: rest), MsgPack.fix_array(len: 2, rest: pair), _opts) do
    {key, new_val} = MsgPack.next(pair)
    {new, appendp} = set_(rest, key, new_val)
    if appendp do
      MsgPack.map32(len: 0x10000 + 1, rest: new)
    else
      MsgPack.map16(len: 15, rest: new)
    end
  end
  def set(MsgPack.map16(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), _opts) do
    {key, new_val} = MsgPack.next(pair)
    {new, appendp} = set_(rest, key, new_val)
    if appendp do
      MsgPack.map16(len: len + 1, rest: new)
    else
      MsgPack.map16(len: len, rest: new)
    end
  end
  def set(MsgPack.map32(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), _opts) do
    {key, new_val} = MsgPack.next(pair)
    {new, appendp} = set_(rest, key, new_val)
    if appendp do
      MsgPack.map32(len: len + 1, rest: new)
    else
      MsgPack.map32(len: len, rest: new)
    end
  end
  def set(@nil_value, MsgPack.fix_array(len: 2, rest: pair), _opts) do
    MsgPack.fix_map(len: 1, rest: pair)
  end

  defp set_(bin, key, new_val) do
    set_(bin, key, new_val, "")
  end

  defp set_("", key, new_val, acc) do
    {acc <> key <> new_val, true}
  end
  defp set_(bin, key, new_val, acc) do
    sz = byte_size(key)
    case bin do
      << ^key :: [binary, size(sz)], rest :: binary >> ->
        {_old_val, rest} = MsgPack.next(rest)
        {acc <> key <> new_val <> rest, false}
      _ ->
        {another_key, bin} = MsgPack.next(bin)
        {val, bin} = MsgPack.next(bin)
        set_(bin, key, new_val, acc <> another_key <> val)
    end
  end

  @args 2
  def update(MsgPack.fix_map(len: 15, rest: rest), MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, appendp} = update_(rest, key, update, opts)
    if appendp do
      MsgPack.map16(len: 16, rest: new)
    else
      MsgPack.fix_map(len: 15, rest: new)
    end
  end
  def update(MsgPack.fix_map(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, appendp} = update_(rest, key, update, opts)
    if appendp do
      MsgPack.fix_map(len: len + 1, rest: new)
    else
      MsgPack.fix_map(len: len, rest: new)
    end
  end
  def update(MsgPack.map16(len: 0x10000, rest: rest), MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, appendp} = update_(rest, key, update, opts)
    if appendp do
      MsgPack.map32(len: 0x10000 + 1, rest: new)
    else
      MsgPack.map16(len: 15, rest: new)
    end
  end
  def update(MsgPack.map16(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, appendp} = update_(rest, key, update, opts)
    if appendp do
      MsgPack.map16(len: len + 1, rest: new)
    else
      MsgPack.map16(len: len, rest: new)
    end
  end
  def update(MsgPack.map32(len: len, rest: rest), MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, appendp} = update_(rest, key, update, opts)
    if appendp do
      MsgPack.map32(len: len + 1, rest: new)
    else
      MsgPack.map32(len: len, rest: new)
    end
  end
  def update(@nil_value, MsgPack.fix_array(len: 2, rest: pair), opts) do
    {key, update} = MsgPack.next(pair)
    {update, ""} = MsgPack.unpack(update)
    {new, true} = update_("", key, update, opts)
    MsgPack.fix_map(len: 1, rest: new)
  end

  defp update_(bin, key, update, opts) do
    update_(bin, key, update, "", opts)
  end

  defp update_("", key, update, acc, opts) do
    {acc <> key <> Genomu.Operation.apply(update, @nil_value, opts), true}
  end
  defp update_(bin, key, update, acc, opts) do
    sz = byte_size(key)
    case bin do
      << ^key :: [binary, size(sz)], rest :: binary >> ->
        {old_val, rest} = MsgPack.next(rest)
        {acc <> key <> Genomu.Operation.apply(update, old_val, opts) <> rest, false}
      _ ->
        {another_key, bin} = MsgPack.next(bin)
        {val, bin} = MsgPack.next(bin)
        update_(bin, key, update, acc <> another_key <> val, opts)
    end
  end

  @args 0
  def keys(MsgPack.fix_map(rest: rest), _no_arg, _opts) do
    keys_(rest)
  end
  def keys(MsgPack.map16(rest: rest), _no_arg, _opts) do
    keys_(rest)
  end
  def keys(MsgPack.map32(rest: rest), _no_arg, _opts) do
    keys_(rest)
  end

  defp keys_(bin) do
    keys_(bin, 0, "")
  end

  defp keys_("", length, acc), do: arr_(length, acc)
  defp keys_(bin, length, acc) do
    {key, bin} = MsgPack.next(bin)
    {_val, bin} = MsgPack.next(bin)
    keys_(bin, length + 1, acc <> key)
  end

  @args 0
  def values(MsgPack.fix_map(rest: rest), _no_arg, _opts) do
    values_(rest)
  end
  def values(MsgPack.map16(rest: rest), _no_arg, _opts) do
    values_(rest)
  end
  def values(MsgPack.map32(rest: rest), _no_arg, _opts) do
    values_(rest)
  end

  defp values_(bin) do
    values_(bin, 0, "")
  end

  defp values_("", length, acc), do: arr_(length, acc)
  defp values_(bin, length, acc) do
    {_key, bin} = MsgPack.next(bin)
    {val, bin} = MsgPack.next(bin)
    values_(bin, length + 1, acc <> val)
  end


  @args 0
  def size(MsgPack.fix_map(len: len), _no_arg, _opts) do
    MsgPack.pack(len)
  end
  def size(MsgPack.map16(len: len), _no_arg, _opts) do
    MsgPack.pack(len)
  end
  def size(MsgPack.map32(len: len), _no_arg, _opts) do
    MsgPack.pack(len)
  end

  defp arr_(length, bin) when length < 16 do
    MsgPack.fix_array(len: length, rest: bin)
  end
  defp arr_(length, bin) when length < 0x10000 do
    MsgPack.array16(len: length, rest: bin)
  end
  defp arr_(length, bin) do
    MsgPack.array32(len: length, rest: bin)
  end

end