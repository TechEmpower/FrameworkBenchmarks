use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
config :hello, Hello.Endpoint,
  secret_key_base: "Z18ZjzZslFpKd8HB41IljqMavPiOKVF9y1DIQ+S2Ytg7Op0EIauwJgd7mtRStssx"

# Configure your database
config :hello, Hello.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  hostname: "127.0.0.1",
  pool_size: 256
