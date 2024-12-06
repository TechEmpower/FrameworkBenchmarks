import Config

config :hello, HelloWeb.Endpoint,
  adapter: Bandit.PhoenixAdapter,
  http: [
    port: 4000,
    ip: {0, 0, 0, 0},
    http_options: [
      compress: false,
      log_protocol_errors: :verbose
    ],
  ],
  debug_errors: true,
  code_reloader: true,
  cache_static_lookup: false

# Watch static and templates for browser reloading.
config :hello, HelloWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"lib/hello_web/(live|views)/.*(ex)$",
      ~r"lib/hello_web/templates/.*(eex)$"
    ]
  ]

config :hello, Hello.Repo,
  username: "postgres",
  password: "postgres",
  database: "hello_world",
  hostname: "localhost"

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"
