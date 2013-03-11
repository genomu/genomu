ExUnit.start

defmodule Genomu.TestCase do
  use ExUnit.CaseTemplate

  @http_port 19119
  @handoff_port 18099
  @port 19101

  using(options) do
    quote do
      @options unquote(options)
      import Genomu.TestCase
      use Genomu.Client
    end
  end

  def qc_output('.', []) do
  end
  def qc_output('~n', []) do
  end
  def qc_output('OK: Passed ~b test(s).~n', _) do
  end
  def qc_output(msg, args) do
    :io.format(msg, args)
  end

  defmacro qc(do: body) do
    quote do
      f =
      fn() ->
        unquote(body)
      end
      assert Proper.quickcheck(f.(), numtests: 100, on_output: function(Genomu.TestCase.qc_output/2)) == true
    end
  end

  def config(:http_port), do: @http_port
  def config(:handoff_port), do: @handoff_port
  def config(:port), do: @port

  def start_db do
    :net_kernel.start([:genomu_test, :shortnames])
    data_dir = Path.expand("../.test",__FILE__)
    File.mkdir_p(data_dir)
    :application.load(:riak_core)
    :application.set_env(:riak_core, :handoff_port, @handoff_port)
    :application.set_env(:riak_core, :cluster_name, 'test')
    :application.set_env(:riak_core, :ring_state_dir, Path.join(data_dir, "ring") |> to_char_list)
    :application.load(:genomu)
    :application.set_env(:genomu, :http_port, @http_port)
    :application.set_env(:genomu, :port, @port)
    :application.set_env(:genomu, :data_dir, data_dir)
    :application.set_env(:genomu, :root_channels, 4)
    :application.load(:lager)
    :application.set_env(:lager, :handlers, [])
    unless nil?(System.get_env("GENOMU_TEST_LOG")) do
      :application.set_env(:lager, :handlers, 
                           [{:lager_console_backend, 
                              binary_to_atom(System.get_env("GENOMU_TEST_LOG"))}])
    end
    :application.set_env(:kernel, :error_logger, false)
    :application.load(:sasl)
    :application.set_env(:sasl, :sasl_error_logger, false)    
    Application.start(:genomu)
    Application.start(:genomu_client)
    :riak_core.wait_for_application(:genomu)
    :riak_core.wait_for_service(:genomu)
  end

  def connection do
    {:ok, c} = Genomu.Client.connect(host: "localhost", port: config(:port))
    c
  end

  def stop_db do
    Application.stop(:genomu)
    Application.stop(:cowboy)
    Application.stop(:ranch)
    :code.delete Genomu.Commands
    :code.purge Genomu.Commands
    data_dir = Path.expand("../.test",__FILE__)
    File.rm_rf(data_dir)
    :net_kernel.stop
  end
end