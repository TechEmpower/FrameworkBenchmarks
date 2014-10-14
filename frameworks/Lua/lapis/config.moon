import config from require "lapis.config"

config "development", ->

config "production", ->
  port 80
  num_workers 4
  lua_code_cache "on"
  postgres {
    backend: "pgmoon"
    database: "hello_world"
    user: "postgres"
    host: "DBHOSTNAME"
  }
