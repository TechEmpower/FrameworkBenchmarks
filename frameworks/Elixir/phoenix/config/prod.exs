use Mix.Config

config :hello, Hello.Endpoint,
  url: [host: "0.0.0.0"],
  http: [port: 8080, protocol_options: [max_keepalive: 5_000_000]],
  cache_static_lookup: false,
  server: true

config :hello, Hello.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  hostname: "127.0.0.1",
  pool_size: 20

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

config :logger, level: :error
