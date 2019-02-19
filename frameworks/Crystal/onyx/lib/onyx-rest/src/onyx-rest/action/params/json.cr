require "json"
require "../../error"

module Onyx::REST::Action
  # Define JSON params which would be deserialized from the request body only if
  # its "Content-Type" header is "application/json". The serialization is powered by
  # stdlib's [`JSON::Serializable`](https://crystal-lang.org/api/latest/JSON/Serializable.html).
  #
  # ```
  # struct UpdateUser
  #   include Onyx::REST::Action
  #
  #   params do
  #     path do
  #       type id : Int32
  #     end
  #
  #     json do
  #       type user do
  #         type email : String?
  #         type username : String?
  #       end
  #     end
  #   end
  #
  #   def call
  #     if json = params.json
  #       pp! json.user.email
  #       pp! json.user.username
  #     end
  #   end
  # end
  # ```
  #
  # ```shell
  # > curl -X POST -H "Content-Type: application/json" -d '{"user":{"email":"foo@example.com"}}' http://localhost:5000/users/1
  # json.user.email    => "foo@example.com"
  # json.user.username => nil
  # ```
  macro json(&block)
    class JSONBodyError < Onyx::REST::Error(400)
    end

    struct JSONBody
      include JSON::Serializable

      {% verbatim do %}
        macro type(argument, **options, &block)
          {% if block %}
            {% unless options.empty? %}
              @[JSON::Field({{**options}})]
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
              include JSON::Serializable

              {% if block.body.is_a?(Expressions) %}
                {% for expression in block.body.expressions %}
                  JSONBody.{{expression}}
                {% end %}
              {% elsif block.body.is_a?(Call) %}
                JSONBody.{{yield.id}}
              {% else %}
                {% raise "BUG: Unhandled block body type #{block.body.class_name}" %}
              {% end %}
            end
          {% elsif argument.is_a?(TypeDeclaration) %}
            {% unless options.empty? %}
              @[JSON::Field({{**options}})]
            {% end %}

            getter {{argument}}
          {% else %}
            {% raise "BUG: Unhandled argument type #{argument.class_name}" %}
          {% end %}
        end
      {% end %}

      {{yield.id}}
    end

    getter json  : JSONBody?

    def initialize(request : HTTP::Request)
      previous_def

      begin
        if request.headers["Content-Type"]?.try &.=~ /^application\/json/
          if body = request.body
            @json = JSONBody.from_json(body.gets_to_end)
          else
            raise JSONBodyError.new("Missing request body")
          end
        end
      rescue ex : JSON::MappingError
        raise JSONBodyError.new(ex.message.not_nil!.lines.first)
      end
    end
  end
end
