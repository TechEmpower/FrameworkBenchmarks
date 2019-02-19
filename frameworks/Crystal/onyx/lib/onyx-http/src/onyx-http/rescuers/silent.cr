require "../rescuer"
require "../../ext/http/server/response/error"

module Onyx::HTTP
  # HTTP handlers which rescue errors.
  module Rescuers
    # A silent rescuer which passes the error to the `#next_handler` if it's present,
    # otherwise prints `"500 Internal Server Error"` into the response body.
    class Silent(T)
      include Rescuer(T)

      # Do nothing.
      def handle(context, error)
        # Do nothing
      end

      # Print `"500 Internal Server Error"` into the response body.
      def fallback(context, error)
        context.response.status_code = 500
        context.response << "500 Internal Server Error"
      end
    end
  end
end
