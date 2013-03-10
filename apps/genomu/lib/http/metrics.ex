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
    lc {Genomu.Metrics, name} = metric inlist :folsom_metrics.get_metrics do
      [{_, opts}] = :folsom_metrics.get_metric_info(metric)
      case opts[:type] do
        :histogram ->
          stats = :folsom_metrics.get_histogram_statistics(metric)
          stats = Keyword.put(stats, :percentile, Enum.map(stats[:percentile], fn({a,b}) -> [a,b] end))
          stats = Keyword.put(stats, :histogram,  Enum.map(stats[:histogram], fn({a,b}) -> [a,b] end))

          {inspect(name), stats}
        _ ->
        {inspect(name),
          :folsom_metrics.get_metric_value(metric)}
      end
    end
    {json |> maybe_jsonp(req), req, state}
  end

end