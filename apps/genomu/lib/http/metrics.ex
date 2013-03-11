defmodule Genomu.HTTP.Metrics do

  use Genomu.HTTP.Resource

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def content_types_provided(req, state) do
    {[{"application/json", :to_json}], req, state}
  end

  def to_json(req, state) do
    mem_metrics = :memsup.get_system_memory_data
    sys_metrics = :folsom_vm_metrics.get_system_info
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
    end ++ 
      [{"Memory", mem_metrics},
       {"Processes", sys_metrics[:process_count]},
       {"CPU", [utilization: :cpu_sup.util, avg1: :cpu_sup.avg1, avg5: :cpu_sup.avg5, avg15: :cpu_sup.avg15]},
      ]
    {json |> maybe_jsonp(req), req, state}
  end

end