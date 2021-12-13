use Mix.Config

config :framework_benchmarks, ecto_repos: [FrameworkBenchmarks.Repo]

import_config "#{Mix.env()}.exs"
