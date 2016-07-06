require "http/server"
require "json"

server = HTTP::Server.new(8080) do |context|
  response = context.response
  response.headers.merge!({"Server": "Crystal", "Date": Time.utc_now.to_s})
  case context.request.path
  when "/json"
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"
    response.print({message: "Hello, World!"}.to_json)
  when "/plaintext"
    response.status_code = 200
    response.headers["Content-Type"] = "text/plain"
    response.print "Hello, World!"
  else
    response.status_code = 404
  end
end

server.listen
