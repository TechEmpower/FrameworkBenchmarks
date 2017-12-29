require "amber"

require "../src/models/*"
require "../src/controllers/*"

Amber::Server.configure do |app|
  app.name = "TFB test app"
  app.color = false
  app.port = 8080
  app.host = "0.0.0.0"
end
