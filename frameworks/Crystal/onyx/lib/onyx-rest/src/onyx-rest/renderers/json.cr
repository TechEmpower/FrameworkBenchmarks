require "onyx-http/ext/http/server/response/error"

require "json"
require "http/server/handler"

require "../ext/http/server/response/view"
require "../error"

module Onyx::REST
  # HTTP handlers which render content.
  module Renderers
    # A JSON renderer. If `::HTTP::Server::Response#error` is present, prints it as a JSON object,
    # otherwise renders `::HTTP::Server::Response#view`, calling `View#to_json` on it.
    # Should be put after router.
    # Calls the next handler if it's present.
    class JSON
      include ::HTTP::Handler

      # :nodoc:
      def call(context)
        context.response.content_type = "application/json; charset=utf-8"

        if error = context.response.error
          json = ::JSON::Builder.new(context.response)

          json.document do
            json.object do
              json.field("error") do
                name = "UnhandledServerError"
                message = "Unhandled server error. If you are the application owner, see the logs for details"
                code = 500
                payload = nil

                case error
                when REST::Error
                  code = error.code
                  name = error.name
                  message = error.message
                  payload = error.payload
                when ::HTTP::Params::Serializable::Error
                  code = 400
                  name = "BadRequest"
                  message = error.message
                  payload = {
                    path: error.path,
                  }
                when HTTP::Router::RouteNotFoundError
                  code = 404
                  name = "RouteNotFound"
                  message = error.message
                  payload = {
                    method: error.method,
                    path:   error.path,
                  }
                end

                context.response.status_code = code

                json.object do
                  json.field "class", name || error.class.name.split("::").last
                  json.field "message", message if message && !message.empty?
                  json.field "code", code
                  json.field "payload", payload if payload
                end
              end
            end
          end
        elsif view = context.response.view
          json = ::JSON::Builder.new(context.response)
          json.document do
            view.to_json(json)
          end
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
