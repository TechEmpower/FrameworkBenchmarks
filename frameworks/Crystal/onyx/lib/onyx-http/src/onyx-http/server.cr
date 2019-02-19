require "http/server"

module Onyx::HTTP
  # The Onyx HTTP server. Basically it is just a wrapper around default `::HTTP::Server`,
  # which logs server start and stop events.
  #
  # ```
  # server = Onyx::HTTP::Server.new(handlers)
  # server.bind_tcp("0.0.0.0", 5000)
  # server.listen
  #
  # #   INFO -- : ⬛ Onyx::HTTP::Server is listening at http://0.0.0.0:5000
  # ^C
  # #   INFO -- : ⬛ Onyx::HTTP::Server is shutting down!
  # ```
  class Server < ::HTTP::Server
    def self.new(handlers : Enumerable, **nargs, &block : ::HTTP::Handler::HandlerProc)
      middleware = ::HTTP::Server.build_middleware(handlers.to_a.map(&.as(::HTTP::Handler)), block)
      new(middleware, **nargs)
    end

    def self.new(*handlers : ::HTTP::Handler, **nargs, &block : ::HTTP::Handler::HandlerProc)
      new(handlers.to_a, **nargs, &block)
    end

    def self.new(**nargs, &block : ::HTTP::Handler::HandlerProc)
      new(block, **nargs)
    end

    def self.new(handlers : Enumerable, **nargs)
      middleware = ::HTTP::Server.build_middleware(handlers.to_a.map(&.as(::HTTP::Handler)))
      new(middleware, **nargs)
    end

    def self.new(*handlers : ::HTTP::Handler, **nargs)
      new(handlers.to_a, **nargs)
    end

    def initialize(
      middleware : ::HTTP::Handler | ::HTTP::Handler::HandlerProc,
      *,
      @name : String = "Onyx::HTTP::Server",
      @logger : ::Logger? = ::Logger.new(STDOUT),
      @logger_severity : ::Logger::Severity = ::Logger::INFO
    )
      super(middleware)
    end

    # Start listening for requests. Blocks the runtime, just like the vanilla `::HTTP::Server`.
    def listen
      if logger = @logger
        io = IO::Memory.new
        io << "⬛".colorize(:green).mode(:bold) << " " << @name
        io << " is listening at ".colorize(:light_gray)
        io << @sockets.join(", ") { |s| format_address(s) }
        logger.log(@logger_severity, io.to_s)
      end

      Signal::INT.trap do
        if logger = @logger
          puts "\n"
          io = IO::Memory.new
          io << "⬛".colorize(:red).mode(:bold) << " " << @name
          io << " is shutting down!".colorize(:light_gray)
          logger.not_nil!.log(@logger_severity, io.to_s)
        end

        exit
      end

      super
    end

    protected def format_address(socket : Socket::Server)
      case socket
      when OpenSSL::SSL::Server then "https://#{socket.local_address}"
      when TCPServer            then "http://#{socket.local_address}"
      when UNIXServer           then "#{socket.path}"
      else                           "?"
      end
    end
  end
end
