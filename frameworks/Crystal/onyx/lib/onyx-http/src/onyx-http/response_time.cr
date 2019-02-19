require "http/server/handler"

module Onyx::HTTP
  # Sets `"X-Response-Time"` header to the integer amount of **microseconds** elapsed
  # to process this request. It is expected to be placed in the very beginning of the middleware.
  class ResponseTime
    include ::HTTP::Handler

    def call(context)
      elapsed = Time.measure do
        call_next(context)
      end

      context.response.headers["X-Response-Time"] = (elapsed.total_milliseconds * 1000).to_i64.to_s
    end
  end
end
