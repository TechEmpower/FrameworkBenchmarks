import config from require "lapis.config"

config "development", ->

config {"production", "development"}, ->
  port 80
  num_workers 4
  lua_code_cache "on"
  logging false
  postgres ->
    backend "pgmoon"
    database "hello_world"
    user "benchmarkdbuser"
    password "benchmarkdbpass"
    host "DBHOSTNAME"
