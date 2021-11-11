use Mix.Config

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :hello, HelloWeb.Endpoint,
  http: [port: 8080],
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
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  hostname: "tfb-database"

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"
