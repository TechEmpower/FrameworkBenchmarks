# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
import Config

config :phoenix, :json_library, Jason

config :hello,
  ecto_repos: [Hello.Repo]

# Configures the endpoint
config :hello, HelloWeb.Endpoint,
  url: [host: "localhost"],
  http: [port: 8080],
  root: Path.expand(__DIR__),
  debug_errors: false,
  secret_key_base: "Z18ZjzZslFpKd8HB41IljqMavPiOKVF9y1DIQ+S2Ytg7Op0EIauwJgd7mtRStssx"

# Configure cache for world entities
config :hello, Hello.WorldCache,
  gc_interval: :timer.hours(1),
  max_size: 1_000_000,
  allocated_memory: 100_000_000,
  gc_cleanup_min_timeout: :timer.seconds(30),
  gc_cleanup_max_timeout: :timer.minutes(30)

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
