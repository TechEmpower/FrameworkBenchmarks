require "http/server/handler"

module Onyx::HTTP
  # Extracted from [kemalyst](https://github.com/kemalyst/kemalyst/blob/master/src/kemalyst/handler/cors.cr) (MIT licensed).
  #
  # Copyright (c) 2016 dru.jensen
  #
  # See [CORS on Wiki](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing).
  class CORS
    include ::HTTP::Handler

    def initialize(
      *,
      @allow_origin = "*",
      @allow_headers = %w(accept content-type),
      @allow_methods = %w(GET HEAD POST DELETE OPTIONS PUT PATCH),
      @allow_credentials = false,
      @max_age = 0
    )
    end

    # :nodoc:
    def call(context)
      begin
        context.response.headers["Access-Control-Allow-Origin"] = @allow_origin

        if @allow_credentials
          context.response.headers["Access-Control-Allow-Credentials"] = "true"
        end

        if @max_age > 0
          context.response.headers["Access-Control-Max-Age"] = @max_age.to_s
        end

        if context.request.method == "OPTIONS"
          context.response.status_code = 200
          response = ""

          if requested_method = context.request.headers["Access-Control-Request-Method"]
            if @allow_methods.includes?(requested_method.strip)
              context.response.headers["Access-Control-Allow-Methods"] = @allow_methods.join(", ")
            else
              context.response.status_code = 403
              response = "Method #{requested_method} not allowed."
            end
          end

          if requested_headers = context.request.headers["Access-Control-Request-Headers"]
            requested_headers.split(",").each do |requested_header|
              if @allow_headers.includes?(requested_header.strip.downcase)
                context.response.headers["Access-Control-Allow-Headers"] = @allow_headers.join(", ")
              else
                context.response.status_code = 403
                response = "Headers #{requested_headers} not allowed."
              end
            end
          end

          context.response.content_type = "text/html; charset=utf-8"
          context.response.print(response)
        else
          call_next(context)
        end
      end
    end
  end
end
