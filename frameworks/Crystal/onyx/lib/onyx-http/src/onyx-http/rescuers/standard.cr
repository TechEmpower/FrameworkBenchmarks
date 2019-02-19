require "colorize"
require "logger"

require "../rescuer"
require "../../ext/http/request/id"

module Onyx::HTTP
  # HTTP handlers which rescue errors.
  module Rescuers
    # A generic handler which rescues all `T`s and logs them colorfully into a standard `::Logger`.
    # It sets `::HTTP::Server::Response#error` to the error instance in `#handle` and
    # then calls the `#next_handler` if it's present, otherwise prints the
    # `"500 Internal Server Error"` message into the response body.
    #
    # It also logs the `::HTTP::Request#id` if it's present.
    # Should be put after `::HTTP::Logger` in the stack.
    #
    # NOTE: It also prints `"500 Internal Server Error"` when route is not found, because the `Router`
    # raises `Router::RouteNotFoundError`, which is a simple `Exception`. You should put
    # `Rescuers::RouteNotFound` *after* standard rescuer as well.
    #
    # ```
    # # This stack will print "500 Internal Server Error" into the response body
    # # if an unhandled error is raised during the processing,
    # # or "404 Route Not Found" on unknown URL path
    # #
    #
    # logger = Onyx::HTTP::Logger.new
    # rescuer = Onyx::HTTP::Rescuers::Standard(Exception).new
    # router_rescuer = Onyx::HTTP::Rescuers::RouteNotFound.new
    # router = Onyx::HTTP::Router.new
    # handlers = [logger, rescuer, router_rescuer, router]
    # ```
    #
    # ```
    # # This stack will call the *renderer* instead of printing into the response body
    # #
    #
    # logger = Onyx::HTTP::Logger.new
    # renderer = Onyx::REST::Renderers::JSON.new
    # rescuer = Onyx::HTTP::Rescuers::Standard.new(renderer)
    # router_rescuer = Onyx::HTTP::Rescuers::RouteNotFound.new(renderer)
    # router = Onyx::HTTP::Router.new
    # handlers = [logger, rescuer, router_rescuer, router, renderer]
    # ```
    class Standard(T)
      include Rescuer(T)

      # A `Logger` to log to. Can be changed in runtime.
      property logger : ::Logger

      # Set *verbose* to `false` to turn off logging errors' backtraces.
      def initialize(
        next_handler : ::HTTP::Handler? = nil,
        *,
        @logger : ::Logger = ::Logger.new(STDERR),
        @verbose : Bool = true
      )
        super(next_handler)
      end

      # Log the *error* into the `#logger`.
      def handle(context, error)
        io = IO::Memory.new

        if id = context.request.id?
          io << "[#{id[0...8]}] ".colorize(:dark_gray)
        end

        io << " ERROR ".rjust(7).colorize.mode(:bold).back(:red)
        io << " " << (error.message || "<Empty message error>")

        if @verbose
          io << "\n\n" << error.inspect_with_backtrace.colorize(:light_gray)
        end

        @logger.error(io.to_s)
      end

      # Print `"500 Internal Server Error"` into the response body.
      def fallback(context, error)
        context.response.status_code = 500
        context.response << "500 Internal Server Error"
      end
    end
  end
end
