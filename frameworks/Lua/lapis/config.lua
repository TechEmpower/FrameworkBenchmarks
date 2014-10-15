local config
do
  local _obj_0 = require("lapis.config")
  config = _obj_0.config
end
config("development", function() end)
return config("production", function()
  port(80)
  num_workers(4)
  lua_code_cache("on")
  return postgres({
    backend = "pgmoon",
    database = "hello_world",
    user = "postgres",
    host = "DBHOSTNAME"
  })
end)
