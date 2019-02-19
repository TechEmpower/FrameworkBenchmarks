require "./silent"
require "../router"

module Onyx::HTTP
  # HTTP handlers which rescue errors.
  module Rescuers
    # A rescuer which rescues `Router::RouteNotFoundError` and passes it
    # to the `#next_handler` if it's present,
    # otherwise prints `"404 Route Not Found: METHOD /path"` into the response body.
    class RouteNotFound < Silent(Router::RouteNotFoundError)
      # Print `"404 Route Not Found: METHOD /path"` into the response body.
      def fallback(context, error)
        context.response.status_code = 404
        context.response << "404 " << error.message
      end
    end
  end
end
