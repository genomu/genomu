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
    [applications: [:exlager, :xup, :genx, :exmsgpack,
                    :mochiweb, :compiler, :syntax_tools, ## riak_core
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
     {:riak_core,     github: "basho/riak_core"},
       {:protobuffs,  github: "basho/erlang_protobuffs"},
       {:basho_stats, github: "basho/basho_stats"},
       {:riak_sysmon, github: "basho/riak_sysmon"},
       {:webmachine,  github: "basho/webmachine"},
         {:mochiweb,  github: "basho/mochiweb", tag: "1.5.1-riak-1.0.x-fixes"},
       {:folsom,      github: "boundary/folsom"},
         {:bear,      github: "boundary/bear", tag: "0.1.1"},
         {:meck,      github: "eproxus/meck"},
       {:poolboy,     github: "devinus/poolboy"},
     {:properex,      github: "yrashk/properex"},
     {:exconfig,      github: "yrashk/exconfig"},
     {:jsx,           github: "talentdeficit/jsx"},
     {:relex,         github: "yrashk/relex"},
    ]
  end

  defp root do
    Path.join([Path.dirname(__FILE__), "..", ".."]) |> Path.expand
  end

end
