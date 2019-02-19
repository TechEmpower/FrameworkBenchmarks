require "http-params-serializable"
require "../../error"

module Onyx::REST::Action
  # Define Form params which would be deserialzed from the request body only if
  # its "Content-Type" header is "application/x-www-form-urlencoded". The serialization is powered by [`HTTP::Params::Serializable`](https://github.com/vladfaust/http-params-serializable).
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
  #     form do
  #       type user do
  #         type email : String?
  #         type username : String?
  #       end
  #     end
  #   end
  #
  #   def call
  #     if form = params.form
  #       pp! form.user.email
  #       pp! form.user.username
  #     end
  #   end
  # end
  # ```
  #
  # ```shell
  # > curl -X POST -H "Content-Type: application/x-www-form-urlencoded" -d "user[email]=foo@example.com" http://localhost:5000/users/42
  # form.user.email    => "foo@example.com"
  # form.user.username => nil
  # ```
  macro form(&block)
    class FormBodyError < Onyx::REST::Error(400)
      def initialize(message : String, @path : Array(String))
        super(message)
      end

      def payload
        {path: @path}
      end
    end

    struct FormParams
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
                  FormParams.{{expression}}
                {% end %}
              {% elsif block.body.is_a?(Call) %}
                FormParams.{{yield.id}}
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

    getter form  : FormParams?

    def initialize(request : ::HTTP::Request)
      previous_def

      begin
        if request.headers["Content-Type"]?.try &.=~ /^application\/x-www-form-urlencoded/
          if body = request.body
            @form = FormParams.from_query(body.gets_to_end)
          else
            raise FormBodyError.new("Missing request body", [] of String)
          end
        end
      rescue ex : ::HTTP::Params::Serializable::Error
        raise FormBodyError.new("Form p" + ex.message.not_nil![1..-1], ex.path)
      end
    end
  end
end
