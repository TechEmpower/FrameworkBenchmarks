require "amber"

require "./controllers/**"
require "./models/**"
require "./views/**"
require "../config/*"

Amber::Server.instance.run
