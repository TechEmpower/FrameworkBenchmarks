local config
config = require("lapis.config").config
config("development", function() end)
return config({
  "production",
  "development"
}, function()
  port(80)
  num_workers(4)
  lua_code_cache("on")
  logging(false)
  return postgres(function()
    backend("pgmoon")
    database("hello_world")
    user("benchmarkdbuser")
    password("benchmarkdbpass")
    return host("DBHOSTNAME")
  end)
end)
