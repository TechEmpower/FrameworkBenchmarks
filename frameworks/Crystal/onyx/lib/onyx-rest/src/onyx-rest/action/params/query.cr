require "http-params-serializable"
require "../../error"

module Onyx::REST::Action
  # Define query params for serialization powered by [`HTTP::Params::Serializable`](https://github.com/vladfaust/http-params-serializable).
  #
  # ```
  # struct IndexUsers
  #   include Onyx::REST::Action
  #
  #   params do
  #     query do
  #       type limit : Int32? = 10
  #       type offset : Int32? = 0
  #     end
  #   end
  #
  #   def call
  #     pp! params.query.limit
  #     pp! params.query.offset
  #   end
  # end
  # ```
  #
  # ```shell
  # > curl http://localhost:5000/users?offset=5
  # params.query.limit  => 10
  # params.query.offset => 5
  # ```
  macro query(&block)
    class QueryParamsError < Onyx::REST::Error(400)
      def initialize(message : String, @path : Array(String))
        super(message)
      end

      def payload
        {path: @path}
      end
    end

    struct Query
      include ::HTTP::Params::Serializable

      {% verbatim do %}
        macro type(argument, **options, &block)
          {% if block %}
            {% unless options.empty? %}
              @[::HTTP::Param({{**options}})]
            {% end %}

            {% if argument.is_a?(Path) %}
              {% raise "Cannot declare namespaced nested query parameter" if argument.names.size > 1 %}

              getter {{argument.names.first.underscore}} : {{argument.names.first.camelcase.id}}
            {% elsif argument.is_a?(Call) %}
              getter {{argument.name.underscore}} : {{argument.name.camelcase.id}}
            {% else %}
              {% raise "BUG: Unhandled argument type #{argument.class_name}" %}
            {% end %}

            {% if argument.is_a?(Path) %}
              struct {{argument.names.first.camelcase.id}}
            {% elsif argument.is_a?(Call) %}
              struct {{argument.name.camelcase.id}}
            {% end %}
              include ::HTTP::Params::Serializable

              {% if block.body.is_a?(Expressions) %}
                {% for expression in block.body.expressions %}
                  Query.{{expression}}
                {% end %}
              {% elsif block.body.is_a?(Call) %}
                Query.{{yield.id}}
              {% else %}
                {% raise "BUG: Unhandled block body type #{block.body.class_name}" %}
              {% end %}
            end
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

    @query = uninitialized Query
    getter query

    def initialize(request : ::HTTP::Request)
      previous_def

      @query = uninitialized Query

      begin
        @query = Query.from_query(request.query.to_s)
      rescue ex : ::HTTP::Params::Serializable::Error
        raise QueryParamsError.new("Query p" + ex.message.not_nil![1..-1], ex.path)
      end
    end
  end
end
