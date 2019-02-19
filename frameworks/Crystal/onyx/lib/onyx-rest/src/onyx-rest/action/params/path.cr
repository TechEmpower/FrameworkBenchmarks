require "http-params-serializable"
require "../../error"

module Onyx::REST::Action
  # Define path params which are usually extracted from the request URL by `Onyx::HTTP::Router`.
  # Serialization is powered by [`HTTP::Params::Serializable`](https://github.com/vladfaust/http-params-serializable).
  #
  # NOTE: It does **not** extracts params from URL by itself, you need to have a router which
  # extracts path params into the `request.path_params` variable, for example, `Onyx::HTTP::Router`;
  # this code only *serializes* them.
  #
  # Path params do not support neither nested nor array values.
  #
  # ```
  # struct GetUser
  #   include Onyx::REST::Action
  #
  #   params do
  #     path do
  #       type id : Int32
  #     end
  #   end
  #
  #   def call
  #     pp! params.path.id
  #   end
  # end
  # ```
  #
  # ```shell
  # > curl http://localhost:5000/users/1
  # params.path.id => 1
  # ```
  macro path(&block)
    class PathParamsError < Onyx::REST::Error(400)
      def initialize(message : String, @path : Array(String))
        super(message)
      end

      def payload
        {path: @path}
      end
    end

    struct Path
      include ::HTTP::Params::Serializable

      {% verbatim do %}
        macro type(argument, **options, &block)
          {% if block %}
            {% raise "Path params do not support nesting" %}
          {% elsif argument.is_a?(TypeDeclaration) %}
            {% unless options.empty? %}
              @[::HTTP::Param({{**options}})]
            {% end %}

            getter {{argument}}
          {% else %}
            {% raise "BUG: Unhandled argument type #{argument.class_name}" %}
          {% end %}
        end
      {% end %}

      {{yield.id}}
    end

    @path = uninitialized Path
    getter path

    def initialize(request : ::HTTP::Request)
      previous_def

      @path = uninitialized Path

      begin
        @path = Path.from_query(request.path_params.join('&'){ |(k, v)| "#{k}=#{v}" })
      rescue ex : ::HTTP::Params::Serializable::Error
        raise PathParamsError.new("Path p" + ex.message.not_nil![1..-1], ex.path)
      end
    end
  end
end
