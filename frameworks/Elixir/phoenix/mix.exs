defmodule Hello.Mixfile do
  use Mix.Project

  def project do
    [
      app: :hello,
      version: "0.1.0",
      elixir: "~> 1.9",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [mod: {Hello, []}, extra_applications: [:logger]]
  end

  defp elixirc_paths(_), do: ["lib", "web"]

  # Specifies your project dependencies
  #
  # Type `mix help deps` for examples and options
  defp deps do
    [
      {:phoenix, "~> 1.4.0"},
      {:phoenix_ecto, "~> 4.0"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, "~> 0.14"},
      {:plug_cowboy, "~> 2.0"},
      {:jason, "~> 1.0"},
      {:phoenix_html, "~> 2.11"},
      {:phoenix_live_reload, "~> 1.2", only: :dev}
    ]
  end
end
