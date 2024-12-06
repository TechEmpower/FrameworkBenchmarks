import Config

config :hello, HelloWeb.Endpoint,
  adapter: Bandit.PhoenixAdapter,
  http: [
    port: 8080,
    ip: {0, 0, 0, 0},
    http_options: [
      compress: false,
      log_protocol_errors: false
    ],
    thousand_island_options: [
      transport_options: [ backlog: 8192 ]
    ],
  ],
  compress: false,
  check_origin: false,
  debug_errors: false,
  code_reloader: false,
  server: true

config :hello, Hello.Repo,
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  hostname: "tfb-database",
  pool_count: 56,
  pool_size: 15,
  queue_target: 5000,
  log: false

config :phoenix, :logger, false

config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :error]
  ],
  level: :error,
  backends: []
