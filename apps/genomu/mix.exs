defmodule Genomu.Mixfile do
  use Mix.Project

  def project do
    [ app:       :genomu,
      version:   version,
      deps:      deps,
      deps_path: Path.join(root, "deps"),
      lockfile:  Path.join(root, "mix.lock"),
    ]
  end

  def version, do: Regex.replace(%r/[\n\r]/,File.read!(Path.expand("../../../VERSION", __FILE__)),"")

  def application do
    [applications: [:gproc,
                    :exlager, :xup, :genx, :exmsgpack,
                    :compiler, :syntax_tools, ## riak_core
                    :bitcask,
                    :jsx,
                    :cowboy, :ranch,
                    :hackney, :dnssd,
                    :os_mon,
                   ] ++ env_applications(Mix.env),
     included_applications: [:riak_core],
     version: version,
     mod: {Genomu.App, []},
    ]
  end

  def env_applications(:dev), do: [:exreloader]
  def env_applications(_), do: []

  defp deps do
    [
     {:genx,          github: "yrashk/genx"},
     {:xup,           github: "yrashk/xup"},
     {:exmsgpack,     github: "yrashk/exmsgpack"},
     {:exlager,       github: "khia/exlager"},
     {:exreloader,    github: "yrashk/exreloader"},
     {:riak_core,     github: "basho/riak_core",
                      compile: "sh -c '#{root}/patches/riak_core_build.sh #{root}'"},
       {:protobuffs,  github: "basho/erlang_protobuffs"},
       {:basho_stats, github: "basho/basho_stats"},
       {:riak_sysmon, github: "basho/riak_sysmon"},
       {:folsom,      github: "boundary/folsom"},
         {:bear,      github: "boundary/bear", tag: "0.1.1"},
         {:meck,      github: "eproxus/meck"},
       {:poolboy,     github: "devinus/poolboy"},
     {:properex,      github: "yrashk/properex"},
     {:exconfig,      github: "yrashk/exconfig"},
     {:jsx,           github: "talentdeficit/jsx"},
     {:cowboy,        github: "extend/cowboy"},
       {:ranch,       github: "extend/ranch", tag: "0.6.1"},
     {:hackney,       github: "benoitc/hackney"},
       {:mimetypes,   github: "spawngrid/mimetypes"},
     {:dnssd,         github: "andrewtj/dnssd_erlang"},
     {:bitcask,       github: "basho/bitcask"},
     {:gproc,         github: "uwiger/gproc"},
    ] ++ deps(Mix.env)
  end

  defp deps(:test) do
    [
     {:genomu_client, github: "genomu/genomu-elixir", branch: "yr-abort"},
    ]
  end
  defp deps(_), do: []

  defp root do
    Path.join([Path.dirname(__FILE__), "..", ".."]) |> Path.expand
  end

end
