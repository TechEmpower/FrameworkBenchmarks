defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [app: :hello,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [mod: {Hello, []},
     applications: [:cowboy, :ranch, :poison]]
  end

  defp deps do
    [{:cowboy, "~> 1.0"},
     {:poison, "~> 1.4.0"}]
  end
end
