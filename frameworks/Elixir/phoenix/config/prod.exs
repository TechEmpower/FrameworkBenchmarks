import Config

config :hello, HelloWeb.Endpoint,
  adapter: Bandit.PhoenixAdapter,
  http: [port: 8080, ip: {0, 0, 0, 0}],
  http_options: [log_protocol_errors: false],
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
  pool_size: 50,
  queue_target: 5000,
  log: false

config :phoenix, :logger, false

config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :error]
  ],
  level: :error,
  backends: []

# ## SSL Support
#
# To get SSL working, you will need to add the `https` key
# to the previous section:
#
#  config:hello, Hello.Endpoint,
#    ...
#    https: [port: 443,
#            keyfile: System.get_env("SOME_APP_SSL_KEY_PATH"),
#            certfile: System.get_env("SOME_APP_SSL_CERT_PATH")]
#
# Where those two env variables point to a file on
# disk for the key and cert.
