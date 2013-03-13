defmodule Genomu.HTTP.Operations do

  use Genomu.HTTP.Resource

  @channel_timeout 60000

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def allowed_methods(req, state) do
    {["GET", "POST"], req, state}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def content_types_accepted(req, state) do
    {[{{"application","json", []}, :process_post},
     {{"application","json", [{"charset","UTF-8"}]}, :process_post},
    ], req, state}
  end

  def to_json(req, state) do
    {path, req} = :cowboy_req.path(req)
    to_json(path, req, state)
  end

  def to_json("/operations", req, state) do
    json = lc module inlist Genomu.Operation.modules do
      details = Genomu.Module.to_json(module, Genomu.system_version)
      {details[:name], details}
    end
    {json |> maybe_jsonp(req), req, state}
  end

  def process_post(req, state) do
    {path, req} = :cowboy_req.path(req)
    process_post(path, req, state)
  end

  defp process_post("/operations", req, state) do
    {:ok, ch} = Genomu.Channel.start
    ch_pid = encode_binary(term_to_binary(ch))
    :timer.apply_after(@channel_timeout, :erlang, :apply, [fn -> Genomu.Channel.discard(ch) end, []])
    json = [channel: ch_pid]
    {:ok, req} = :cowboy_req.reply(200, [], maybe_jsonp(json, req), req)
    {true, req, state}
  end

  defp process_post(<< unquote_splicing(binary_to_list("/operations/")), channel :: binary >>, req, state) do
    ch = decode_binary(channel) |> binary_to_term
    if Process.alive?(ch) do
      {:ok, body, req} = :cowboy_req.body(req)
      json = :jsx.decode(body)
      if json["commit"] == true do
        response = Genomu.Channel.commit(ch, Genomu.Transaction.new)
      else
        key = json["key"]
        op  = encode_operation(json["operation"])
        cmd = command(json["command"], op)
        response = Genomu.Channel.execute(ch, key, cmd, [])
      end
      {:ok, req} = :cowboy_req.reply(200, [], maybe_jsonp(encode_response(response), req), req)
      {true, req, state}
    else
      {false, req, state}
    end
  end

  defp command(0, op), do: Genomu.Command.get(op)
  defp command(1, op), do: Genomu.Command.set(op)
  defp command(2, op), do: Genomu.Command.apply(op)
  defp command(3, ""), do: Genomu.Command.get_operation

  defp encode_operation([{"_genomu", true}|tail]) do
    MsgPack.pack(tail["module"]) <> MsgPack.pack(tail["operation"]) <> 
    MsgPack.pack(encode_operation(tail["argument"]))
  end

  defp encode_operation([{_, _}|_] = map)do
    MsgPack.Map.from_list(lc op inlist map, do: encode_operation(op))
  end

  defp encode_operation(list) when is_list(list) do
    lc op inlist list, do: encode_operation(op)
  end

  defp encode_operation(:null), do: nil
  defp encode_operation(other), do: other

  defp encode_response(:ok), do: true
  defp encode_response(:timeout), do: [error: "timeout"]
  defp encode_response(:abort), do: [error: "abort"]
  defp encode_response({{value, clock}, txn}) do
   {value, _} = MsgPack.unpack(value)
   [result: encode_value(value), vsn: encode_binary(clock), txn: encode_binary(txn)]
  end

  defp encode_value(list) when is_list(list) do
    lc i inlist list, do: encode_value(i)
  end
  defp encode_value(MsgPack.Map[map: map]) do
    encode_value(map)
  end
  defp encode_value({a, b}) do
    {encode_value(a), encode_value(b)}
  end
  defp encode_value(nil), do: :null
  defp encode_value(other), do: other

  defp encode_binary(bin) do
    bc <<b::4>> inbits bin, do: << (integer_to_binary(b, 16)) :: binary >>
  end

  defp decode_binary(bin) do
    bc <<b::[16, bits]>> inbits bin, do: <<binary_to_integer(b, 16)>>
  end
end