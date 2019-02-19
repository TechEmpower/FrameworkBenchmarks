require "onyx-http"
require "./logger"

class Onyx
  {% for method in Onyx::HTTP::Router::HTTP_METHODS %}
    # Define a {{method.upcase.id}} route with block for the top-level `.router`.
    # See `HTTP::Router#{{method.id}}`.
    def self.{{method.id}}(*args, **nargs, &block : HTTP::Server::Context -> Nil)
      Onyx::HTTP::Singleton.instance.router.{{method.id}}(*args, **nargs, &block)
    end

    # Define a {{method.upcase.id}} route for the top-level `.router`.
    # See `HTTP::Router#{{method.id}}`.
    def self.{{method.id}}(*args, **nargs)
      Onyx::HTTP::Singleton.instance.router.{{method.id}}(*args, **nargs)
    end
  {% end %}

  # Draw routes for the top-level `.router`.
  # See `HTTP::Router#draw`.
  def self.draw(&block)
    with Onyx::HTTP::Singleton.instance.router yield
  end

  # The top-level `HTTP::Router` instance.
  def self.router
    Onyx::HTTP::Singleton.instance.router
  end

  # Instantiate an `Onyx::HTTP::Server`, bind it to the *host* and *port* and start listening.
  # Routes for it are defined with top-level `Onyx.get` methods and its siblings.
  #
  # You can insert your custom code into the `&block`.
  # At this point, `handlers` variable will be available.
  macro listen(host = "localhost", port = 5000, reuse_port = true, &block)
    handlers = Onyx::HTTP::Singleton.instance.handlers
    {{yield.id}}
    server = Onyx::HTTP::Server.new(handlers, logger: Onyx.logger)
    server.bind_tcp({{host}}, {{port}}, reuse_port: {{reuse_port}})
    server.listen
  end

  module HTTP
    # The singleton instance of an Onyx HTTP server wrapper.
    # It is **not** initialized unless called.
    class Singleton
      # The singleton instance.
      def self.instance
        @@instance ||= new
      end

      @router = HTTP::Router.new

      # The singleton `HTTP::Router` instance.
      property router

      # The default set of handlers. See its source code to find out which handlers in particular.
      # You can in theory modify these handlers in the `Onyx.listen` block.
      def handlers
        [
          HTTP::ResponseTime.new,
          HTTP::RequestID.new,
          HTTP::Logger.new(
            Onyx.logger,
            query: ENV["CRYSTAL_ENV"] != "production"
          ),
          HTTP::CORS.new,
          HTTP::Rescuers::Standard(Exception).new,
          HTTP::Rescuers::RouteNotFound.new,
          router,
        ].compact!
      end
    end
  end
end
