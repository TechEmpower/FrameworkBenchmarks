import config from require "lapis.config"

config "development", ->
  postgresql_url "postgres://benchmarkdbuser:benchmarkdbpass@127.0.0.1/hello_world"

config "production", ->
  port 80
  num_workers 4
  lua_code_cache "off"
  postgresql_url "postgres://benchmarkdbuser:benchmarkdbpass@127.0.0.1/hello_world"
