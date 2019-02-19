require "uuid"
require "http/server/handler"

require "./ext/http/request/id"

module Onyx::HTTP
  # Sets `::HTTP::Request#id` and `"X-Request-ID"` header to a random `UUID` string.
  # If a request has an ID, it's logged in `Logger` and `Rescuers::Standard` handlers.
  class RequestID
    include ::HTTP::Handler

    def call(context)
      id = UUID.random.to_s
      context.request.id = id
      context.response.headers["X-Request-ID"] = id
      call_next(context)
    end
  end
end
