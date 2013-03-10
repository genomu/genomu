defmodule Genomu.HTTP.Metrics do

  use Genomu.HTTP.Resource

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def to_json(req, state) do
    json =
    lc {Genomu.Metrics, metric} inlist :folsom_metrics.get_metrics do
      {inspect(metric),
        :folsom_metrics.get_metric_value({Genomu.Metrics, metric})}
    end
    {json |> maybe_jsonp(req), req, state}
  end

end