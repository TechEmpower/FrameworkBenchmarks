use Mix.Config

config :hello, Hello.Endpoint,
  url: [host: "localhost", port: 8080],
  http: [port: System.get_env("PORT") || 8080],
  debug_errors: true,
  cache_static_lookup: false

# Enables code reloading for development
config :phoenix, :code_reloader, true

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"
