require "../src/onyx-http"

class SpecServer
  def initialize
    logger = Logger.new(STDOUT, Logger::DEBUG)

    request_id = Onyx::HTTP::RequestID.new
    request_logger = Onyx::HTTP::Logger.new(logger, severity: Logger::DEBUG)
    cors = Onyx::HTTP::CORS.new
    standard_rescuer = Onyx::HTTP::Rescuers::Standard(Exception).new(logger: logger)
    route_not_found_rescuer = Onyx::HTTP::Rescuers::RouteNotFound.new
    router = Onyx::HTTP::Router.new

    router.draw do
      get "/" do
        "Hello Onyx" # It is not expected to actually print anything into the response body
      end

      get "/hello" do |env|
        env.response << "Hello Onyx"
      end

      get "/error" do
        raise "Oops"
      end
    end

    @server = Onyx::HTTP::Server.new([request_id, request_logger, standard_rescuer, route_not_found_rescuer, router], logger: logger)
  end

  getter server
end
