defmodule Genomu.HTTP.Resource do

  defmacro __using__(_) do
    quote do
      import Genomu.HTTP.Resource
    end
  end

  def maybe_jsonp(data, req) do
    encoded = :jsx.to_json(data)
    case :cowboy_req.qs_val("cb", req, nil) do
      {value, _req} when is_binary(value) ->
        "#{value}(#{encoded});"
      _ ->
        encoded
    end
  end

end