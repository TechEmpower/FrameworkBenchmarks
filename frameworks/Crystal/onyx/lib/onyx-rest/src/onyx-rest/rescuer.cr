require "./error"

module Onyx::REST
  # This class exists so the compiler could detect the type at `error.code` call.
  private class NullError < Error(0)
  end

  # An HTTP handler which rescues `REST::Error`.
  #
  # ```
  # renderer = Onyx::REST::Renderers::JSON.new
  # rescuer = Onyx::REST::Rescuer.new(renderer)
  # handlers = [rescuer, router, renderer]
  # ```
  class Rescuer < HTTP::Rescuers::Silent(Error)
    def fallback(context, error)
      context.response.status_code = error.code
      context.response << error.code << " " << error.message
    end
  end
end
