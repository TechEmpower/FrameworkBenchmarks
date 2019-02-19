require "onyx-http/ext/http/server/response/error"

require "http/server/handler"

require "../ext/http/server/response/view"
require "../error"

module Onyx::REST
  # HTTP handlers which render content.
  module Renderers
    # A plain text renderer. If `::HTTP::Server::Response#error` is present, prints it,
    # otherwise renders `::HTTP::Server::Response#view`, calling `View#to_s` on it.
    # Should be put after router.
    # Calls the next handler if it's present.
    class Text
      include ::HTTP::Handler

      # :nodoc:
      def call(context)
        context.response.content_type = "text/plain; charset=utf-8"

        if error = context.response.error
          message = "Internal Server Error"
          code = 500
          payload = nil

          # TODO: Handle ::HTTP::Params::Serializable::Error and HTTP::Router::RouteNotFoundError
          case error
          when REST::Error
            code = error.code
            message = error.message
            payload = error.payload
          end

          context.response.status_code = code
          context.response << code << " " << message
        elsif view = context.response.view
          view.to_text(context.response)
        end

        if self.next
          call_next(context)
        else
          context.response.error = nil
          context.response.view = nil
        end
      end
    end
  end
end
