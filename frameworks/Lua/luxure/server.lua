local lux = require "luxure"
local dkjson = require "dkjson"
local server = lux.Server.new()

server:use(function(req, res, next)
  res:add_header("server", "luxure")
  res:add_header("date", os.date("!%a, %d %b %Y %X GMT"))
  next(req, res)
end)

server:get("/json", function(req, res)
  res:add_header("content-type", "application/json")
  res:send(dkjson.encode({ message = "Hello, World!" }))
end)

server:get("/plaintext", function(req,res)
  res:add_header("content-type", "text/plain")
  res:send("Hello, World!")
end)

server:listen(8080)
server:run()
