require "http/server/response"
require "../../../view"

# Extensions to the standard `HTTP` module.
module HTTP
  # Extensions to the standard `HTTP::Server` class.
  class Server
    # Extensions to the standard `HTTP::Server::Response` class.
    class Response
      # A view to render.
      property view : Onyx::REST::View?
    end
  end
end
