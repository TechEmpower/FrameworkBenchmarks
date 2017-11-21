require "amber"

require "./models/**"
require "./controllers/**"

require "../config/*"

Amber::Server.instance.run
