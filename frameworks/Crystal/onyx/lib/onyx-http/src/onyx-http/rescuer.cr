require "http/server/handler"
require "./ext/http/server/response/error"

# Rescues `T`. Firstly calls `#handle` to handle the error (e.g. log it).
# Then if `#next_handler` is present, calls `#before_next_handler` and then the next handler itself.
# Otherwise calls `#fallback`.
#
# See `Rescuers::Standard` and `Rescuers::Silent`.
module Onyx::HTTP::Rescuer(T)
  include ::HTTP::Handler

  # A handler to call when a error is rescued. If it's missing, calls `#fallback`.
  property next_handler : ::HTTP::Handler?

  # Initialize with a *next_handler* to call when a error is rescued.
  def initialize(@next_handler : ::HTTP::Handler? = nil)
  end

  # :nodoc:
  def call(context)
    call_next(context)
  rescue error : T
    handle(context, error)

    if next_handler = @next_handler
      before_next_handler(context, error)
      next_handler.call(context)
    else
      fallback(context, error)
    end
  end

  # Process the error before further handling. A good example is logging it.
  abstract def handle(context : ::HTTP::Server::Context, error : T)

  # Called only if `#next_handler` is set just before it's called.
  # It does `context.response.error = error` by default.
  def before_next_handler(context : ::HTTP::Server::Context, error : T)
    context.response.error = error
  end

  # Called if no `#next_handler` is set.
  abstract def fallback(context : ::HTTP::Server::Context, error : T)
end

require "./rescuers/*"
