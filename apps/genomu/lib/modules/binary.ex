defmodule Genomu.Module.Binary do
  use Genomu.Module, id: 1, name: :binary
  @moduledoc """
  Binary operations
  """

  @doc """
  Append a binary
  """
  @args 1
  def append(value, arg) do
    MsgPack.pack(extract_bin(value) <> extract_bin(arg))
  end

  @args 2
  def range(value, MsgPack.fix_array(len: 2, rest: rest)) do
    {start_pos, rest} = MsgPack.next(rest)
    {start_pos, ""} = MsgPack.unpack(start_pos)
    {end_pos, ""} = MsgPack.next(rest)
    MsgPack.pack(range_1(value, start_pos, end_pos))
  end

  defp range_1(value, start_pos, MsgPack.uint7(value: end_pos)) do
    range_1_pos(value, start_pos, end_pos)
  end
  defp range_1(value, start_pos, MsgPack.uint8(value: end_pos)) do
    range_1_pos(value, start_pos, end_pos)
  end
  defp range_1(value, start_pos, MsgPack.uint16(value: end_pos)) do
    range_1_pos(value, start_pos, end_pos)
  end
  defp range_1(value, start_pos, MsgPack.uint32(value: end_pos)) do
    range_1_pos(value, start_pos, end_pos)
  end
  defp range_1(value, start_pos, MsgPack.uint64(value: end_pos)) do
    range_1_pos(value, start_pos, end_pos)
  end

  defp range_1_pos(value, start_pos, end_pos) do
    bin = extract_bin(value)
    :binary.part(bin, start_pos, end_pos - start_pos + 1)
  end

  @args 0
  def size(MsgPack.fix_raw(len: len), _) do
    MsgPack.pack(len)
  end
  def size(MsgPack.raw16(len: len), _) do
    MsgPack.pack(len)
  end
  def size(MsgPack.raw32(len: len), _) do
    MsgPack.pack(len)
  end
  def size(MsgPack.atom_nil, _) do
    MsgPack.pack(0)
  end


  defp extract_bin(MsgPack.atom_nil), do: ""
  defp extract_bin(MsgPack.fix_raw(value: value)), do: value
  defp extract_bin(MsgPack.raw16(value: value)), do: value
  defp extract_bin(MsgPack.raw32(value: value)), do: value


end