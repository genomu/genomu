defmodule Genomu.Config do
  use ExConfig.Object

  @shortdoc "Node name"
  defproperty node_name, default: "genomu"
  @shortdoc "Riak Core handoff port"
  defproperty handoff_port, default: 8099
  @shortdoc "Data directory where Genomu will store all files"
  defproperty data_dir, default: "data"
  @shortdoc "Log files directory"
  defproperty log_dir, default: "log"
  @shortdoc "Suppresses console logging if true"
  defproperty quiet_mode, default: false

  def sys_config(config) do
    [
     genomu: [
       data_dir: Path.join(config.data_dir, to_binary(config.node_name)),
     ],
     riak_core: [
       ring_state_dir: to_char_list(Path.join([config.data_dir, to_binary(config.node_name), "ring"])),
       vnode_modules: [{:kernel, Genomu.VNode}],
       handoff_port: config.handoff_port
     ],
     lager:
       [handlers:
          if config.quiet_mode do
            []
          else
            [{:lager_console_backend, :info}]
          end ++
          [
           {:lager_file_backend, [
            {to_char_list(Path.join([config.log_dir, to_binary(config.node_name), "debug.log"])), :debug, 10485760, '$D0', 5},
            {to_char_list(Path.join([config.log_dir, to_binary(config.node_name), "error.log"])), :error, 10485760, '$D0', 5},
            {to_char_list(Path.join([config.log_dir, to_binary(config.node_name), "console.log"])), :info, 10485760, '$D0', 5},
          ]
         }
        ],
        error_logger_redirect: true
       ],
      kernel: [error_logger: false],
      sasl: [sasl_error_logger: false]
    ]
  end

  def sys_config!(filename, config) do
    File.write!(filename, :io_lib.format("~p.~n",[sys_config(config)]))
  end
end