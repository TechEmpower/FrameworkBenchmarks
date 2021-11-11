defmodule FrameworkBenchmarks.MixProject do
  use Mix.Project

  def project do
    [
      app: :framework_benchmarks,
      version: "0.1.0",
      elixir: "~> 1.9",
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
      {:plug_cowboy, "~> 2.0"},
      {:eljiffy, "~> 1.3.0"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, ">= 0.0.0"},
      {:cachex, "~> 3.2"},
      {:phoenix_html, "~> 2.13"},
      {:ucol, "~> 2.0"}
    ]
  end
end
