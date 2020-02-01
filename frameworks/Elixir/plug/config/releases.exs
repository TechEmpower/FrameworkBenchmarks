import Config

config :logger, level: :error

config :framework_benchmarks, FrameworkBenchmarks.Repo,
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  hostname: "tfb-database",
  port: 5432,
  pool_size: 300,
  queue_target: 5000,
  queue_interval: 5000,
  log: false
