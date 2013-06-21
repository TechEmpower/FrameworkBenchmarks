local config
do
  local _table_0 = require("lapis.config")
  config = _table_0.config
end
config("development", function()
  return postgresql_url("postgres://benchmarkdbuser:benchmarkdbpass@127.0.0.1/hello_world")
end)
return config("production", function()
  port(80)
  num_workers(4)
  lua_code_cache("off")
  return postgresql_url("postgres://benchmarkdbuser:benchmarkdbpass@127.0.0.1/hello_world")
end)
