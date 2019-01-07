use Mix.Config

config :hello, http_ip: {127, 0, 0, 1}
config :hello, http_port: 8080
config :hello, compress_body: true
config :hello, num_acceptors: 128
config :hello, max_connections: 32000
config :hello, max_keepalive: 1024
config :logger,
  level: :info,
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

