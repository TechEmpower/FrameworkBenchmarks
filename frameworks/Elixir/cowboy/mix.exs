defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [app: :hello,
     version: "0.1.0",
     elixir: "~> 1.4",
     deps: deps()]
  end

  def application do
    [mod: {Hello, []},
     applications: [:cowboy, :poison, :ranch]]
  end

  defp deps do
    [{:cowboy, "~> 1.0"},
     {:poison, "~> 1.4.0"},
     {:ranch, "~> 1.3.2"}]
  end
end
