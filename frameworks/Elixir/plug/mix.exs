defmodule FrameworkBenchmarks.MixProject do
  use Mix.Project

  def project do
    [
      app: :framework_benchmarks,
      version: "1.1.0",
      elixir: "~> 1.14",
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
      {:ecto_sql, "~> 3.8"},
      {:postgrex, "~> 0.16"},
      {:cachex, "~> 3.4"},
      {:phoenix_html, "~> 3.2"}
    ]
  end
end
