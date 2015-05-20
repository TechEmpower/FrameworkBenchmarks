require "http/server"

server = HTTP::Server.new(8080) do |request|
  HTTP::Response.ok "text/plain", "Hello world! The time is #{Time.now}"
end

puts "Crystal-Moonshine listening port 8080"
server.listen