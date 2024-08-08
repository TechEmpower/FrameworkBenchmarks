defmodule FrameworkBenchmarks.MixProject do
  use Mix.Project

  def project do
    [
      app: :framework_benchmarks,
      version: "1.1.1",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:cachex, :logger],
      mod: {FrameworkBenchmarks.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:plug_cowboy, "~> 2.5"},
      {:jason, "~> 1.4"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:cachex, "~> 3.6"},
      {:phoenix_html, "~> 4.1"}
    ]
  end
end
