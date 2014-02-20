defmodule Trackr.Mixfile do
  use Mix.Project

  def project do
    [ 
      app: :trackr,
      version: "0.0.1",
      elixir: "~> 0.12.1",
      deps: deps 
    ]
  end

  # Configuration for the OTP application
  def application do
    [ 
      mod: { Trackr, [] },
      applications: [
        :mnesia,
        :yamler,
        :cowboy, 
        :exreloader, 
        :raven, 
        :poolboy,
        :hottub
      ] 
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :exreloader,           [github: "yrashk/exreloader"]},
      { :bullet,               [github: "extend/bullet",                  ref: "d907d982d7ac694facab641056880e2ed1c5d2f7"]},
      { :lager,                [github: "basho/lager",                    ref: "822062478a223313dce30e5a45e30a50a4b7dc4e"]},
      { :raven,                [github: "runway20/raven-erlang",          ref: "e273bfc156ead2ddde790187fa678d09edf7ba88"]},
      { :erlson,               [github: "alavrik/erlson"]},
      { :yamler,               [github: "goertzenator/yamler",            ref: "c0ec227111cfd481c4d2a2895bfef7eea42cbc6b"]},
      { :poolboy,              [github: "devinus/poolboy",                ref: "91b68de43d49bcccf2631016febda70b20d7aa75"]},
      { :geoip,                [github: "julienmarie/erlang-geoip",              ref: "ba8e98386a84eddd251e20772c5e367fa7a64ed1"]},
      { :eredis,               [github: "wooga/eredis" ]},
      { :hottub,               [github: "bfrog/hottub", ref: "7639b10e23347caa52de3db5a3123d0a93b850a6"]},
      { :hash_ring,            [github: "chrismoos/hash-ring", ref: "58683855b2870ad842dc0078239aaf277e533225"]}
    ]
  end
end
