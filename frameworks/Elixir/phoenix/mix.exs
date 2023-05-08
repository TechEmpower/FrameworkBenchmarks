defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [
      app: :hello,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [
      mod: {Hello.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies
  #
  # Type `mix help deps` for examples and options
  defp deps do
    [
      {:bandit, ">= 0.6.2"},
      {:gettext, "~> 0.20"},
      {:ecto_sql, "~> 3.7"},
      {:jason, "~> 1.2"},
      {:phoenix, "~> 1.7"},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_html, "~> 3.2"},
      {:phoenix_live_view, "~> 0.18"},
      {:plug_cowboy, "~> 2.5"},
      {:postgrex, "~> 0.15"},
      {:nebulex, "~> 2.4"}
    ]
  end
end
