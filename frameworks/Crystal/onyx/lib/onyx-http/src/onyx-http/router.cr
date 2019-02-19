require "radix"

require "http/server/handler"
require "http/web_socket"

require "./ext/http/request/path_params"

module Onyx::HTTP
  # Routes a request's path, then updates extracted path params to `::HTTP::Request#path_params`,
  # executes the matching proc and calls the next handler if it's present.
  #
  # Raises `RouteNotFoundError` if no route is found for the URL path,
  # so it should be with a `Rescuer` in the stack.
  #
  # ```
  # router = Onyx::HTTP::Router.new do
  #   get "/" do |env|
  #     env.response << "Hello world!"
  #   end
  #
  #   ws "/" do |socket, env|
  #     socket.on_message do |message|
  #       # ...
  #     end
  #   end
  # end
  #
  # router_rescuer = Onyx::HTTP::Rescuers::RouteNotFound.new
  #
  # server = Onyx::HTTP::Server.new([router_rescuer, router])
  # ```
  class Router
    include ::HTTP::Handler

    private alias ContextProc = Proc(::HTTP::Server::Context, Nil)
    private alias Node = ContextProc | ::HTTP::WebSocketHandler

    # :nodoc:
    HTTP_METHODS = %w(get post put patch delete options)
    @tree = Radix::Tree(Node).new
    @hash = {} of String => Node

    # Initialize a new router and yield it. You should then define routes in the *&block*.
    #
    # ```
    # # The simplest router
    # router = Onyx::HTTP::Router.new do
    #   get "/" do |env|
    #     env.response << "Hello world!"
    #   end
    # end
    # ```
    def self.new
      instance = new
      with instance yield
      instance
    end

    # Lookup for a route, update `context.request.path_params` and call the matching proc,
    # raising `RouteNotFound` otherwise. Calls the next handler if it's present.
    def call(context)
      if context.request.headers.includes_word?("Upgrade", "Websocket")
        path = "/ws" + context.request.path.rstrip('/')
        result = lookup(path)
      else
        path = "/" + context.request.method.downcase + context.request.path.rstrip('/')
        result = lookup(path)
      end

      begin
        if proc = result.payload
          if params = result.params
            context.request.path_params = params
          end

          proc.call(context)

          if self.next
            call_next(context)
          else
            context.response.error = nil
          end
        else
          raise RouteNotFoundError.new(context)
        end
      end
    end

    # Yield `with` self.
    #
    # ```
    # router.draw do
    #   post "/" { }
    #   get "/" { }
    # end
    # ```
    def draw(&block)
      with self yield
    end

    # Draw a route for *path* and *methods*.
    #
    # ```
    # router = Onyx::HTTP::Router.new do
    #   on "/foo", methods: %w(get post) do |env|
    #     env.response << "Hello from #{env.request.method} /foo!"
    #   end
    # end
    # ```
    def on(path, methods : Array(String), &proc : ::HTTP::Server::Context -> Nil)
      methods.map(&.downcase).each do |method|
        add("/" + method + path, proc)
      end
    end

    {% for method in HTTP_METHODS %}
      # Draw a route for *path* with `{{method.upcase.id}}` method.
      #
      # ```
      # router = Onyx::HTTP::Router.new do
      #   {{method.id}} "/bar" do |env|
      #     env.response << "Hello from {{method.upcase.id}} /bar!"
      #   end
      # end
      # ```
      def {{method.id}}(path, &proc : ::HTTP::Server::Context -> Nil)
        on(path, [{{method}}], &proc)
      end
    {% end %}

    # Draw a WebSocket route for *path*.
    #
    # A request is currently determined as websocket by `"Upgrade": "Websocket"` header.
    #
    # ```
    # router = Onyx::HTTP::Router.new do
    #   ws "/foo/:bar" do |socket, env|
    #     socket.send("Hello WS!")
    #   end
    # end
    # ```
    def ws(path, &proc : ::HTTP::WebSocket, ::HTTP::Server::Context -> Nil)
      add("/ws" + path, ::HTTP::WebSocketHandler.new(&proc))
    end

    # Raised if a duplicate route is found.
    class DuplicateRouteError < Exception
      # The route which caused the error.
      getter route : String

      # :nodoc:
      def initialize(@route)
        super("Duplicate route found: #{route}")
      end
    end

    # Raised if route is not found for this request. Should be rescued in the stack,
    # presumably by `Rescuers::RouteNotFound`.
    class RouteNotFoundError < Exception
      # The request method.
      getter method : String

      # The request path.
      getter path : String

      # :nodoc:
      def initialize(context : ::HTTP::Server::Context)
        @method = context.request.method
        @path = context.request.path
        super("Route Not Found: #{@method} #{@path}")
      end
    end

    protected def add(path, node)
      if path.includes?(':')
        @tree.add(path.rstrip('/'), node)
      else
        raise DuplicateRouteError.new(path) if @hash.has_key?(path)
        @hash[path.rstrip('/')] = node
      end
    rescue Radix::Tree::DuplicateError
      raise DuplicateRouteError.new(path)
    end

    private struct Result
      getter payload : Node?
      getter params : Hash(String, String)? = nil

      def initialize(@payload : Node?)
      end

      def initialize(result : Radix::Result)
        if result.found?
          @payload = result.payload
          @params = result.params
        end
      end
    end

    protected def lookup(path)
      Result.new(@hash.fetch(path) do
        @tree.find(path)
      end)
    end
  end
end
