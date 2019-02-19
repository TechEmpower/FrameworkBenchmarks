require "./http"
require "onyx-rest"
require "./logger"

class Onyx
  # Select a renderer for `Onyx::REST` top-level server instance. Can be ither `:text` or `:json`.
  macro render(type)
    {% if type.id == "json".id %}
      require "onyx-rest/renderers/json"
      Onyx::HTTP::Singleton.instance.renderer = Onyx::REST::Renderers::JSON.new.as(HTTP::Handler)
    {% elsif type.id == "text".id %}
      require "onyx-rest/renderers/text"
      Onyx::HTTP::Singleton.instance.renderer = Onyx::REST::Renderers::Text.new.as(HTTP::Handler)
    {% else %}
      {% raise %Q[Unknown REST renderer #{type} (valid values are "text" and "json")] %}
    {% end %}
  end

  module HTTP
    class Singleton
      @renderer : ::HTTP::Handler | Nil = nil

      # The singleton renderer instance.
      property renderer

      def handlers
        [
          HTTP::ResponseTime.new,
          HTTP::RequestID.new,
          HTTP::Logger.new(
            Onyx.logger,
            query: ENV["CRYSTAL_ENV"] != "production"
          ),
          HTTP::CORS.new,
          HTTP::Rescuers::Standard(Exception).new(renderer),
          HTTP::Rescuers::RouteNotFound.new(renderer),
          REST::Rescuer.new(renderer),
          router,
          renderer,
        ].compact!
      end
    end
  end
end
