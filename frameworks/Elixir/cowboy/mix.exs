defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [app: :hello,
     version: "0.1.0",
     elixir: "~> 1.7.4",
     deps: deps()]
  end

  def application do
    [mod: {Hello, []},
     applications: [:cowboy, :poison, :ranch]]
  end

  defp deps do
    [{:cowboy, "~> 2.6.1"},
     {:poison, "~> 3.1"},
     {:ranch, "~> 1.7.1"}]
  end
end
