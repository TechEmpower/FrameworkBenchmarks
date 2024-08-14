defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [
      app: :hello,
      version: "1.1.1",
      elixir: "~> 1.17",
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
      {:bandit, "~> 1.0.0"},
      {:gettext, "~> 0.20"},
      {:ecto_sql, "~> 3.10"},
      {:jason, "~> 1.2"},
      {:phoenix, "~> 1.7"},
      {:phoenix_live_view, "~> 0.18"},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_html, "~> 4.1"},
      {:plug_cowboy, "~> 2.5"},
      {:postgrex, ">= 0.0.0"},
      {:nebulex, "~> 2.6"}
    ]
  end
end
