require "http/params"
require "./http-params-serializable/*"

# Including this module will make an object serializable to and from HTTP params query.
# It adds a `.from_query(query : String)` and `#to_query : String` methods.
module HTTP::Params::Serializable
  # Build query path from a tuple of *path* elements.
  #
  # ```
  # build_path("foo", "bar", "") # => "foo[bar][]"
  # ```
  def self.build_path(*path : *T) : String forall T
    {% if T.size > 2 %}
      "#{path[0]}[#{path[1..-1].join("][")}]"
    {% elsif T.size == 2 %}
      "#{path[0]}[#{path[1]}]"
    {% elsif T.size == 1 %}
      path[0].to_s
    {% else %}
      ""
    {% end %}
  end

  # Build query path from an array of *path* elements.
  #
  # ```
  # build_path(["foo", "bar", ""]) # => "foo[bar][]"
  # ```
  def self.build_path(path : Array(String)) : String
    if path.size > 2
      "#{path[0]}[#{path[1..-1].join("][")}]"
    elsif path.size == 2
      "#{path[0]}[#{path[1]}]"
    elsif path.size == 1
      path[0].to_s
    else
      ""
    end
  end

  # Split query *path* into path elements.
  #
  # ```
  # "foo[bar][]".split # => ["foo", "bar", ""]
  # ```
  def self.split_path(path : String) : Array(String)
    if path.size <= 1
      return [path]
    end

    path.split("][").map do |elem|
      elem[(elem[0] == '[' ? 1 : 0)..(elem[-1] == ']' ? -2 : -1)]
    end
  end

  # Serialalize `self` into an HTTP params query with the *builder* at *key*.
  # See `#to_query` for instance variable rules.
  def to_http_param(builder : Builder, key : String? = nil)
    {% for ivar, i in @type.instance_vars %}
      if var = @{{ivar.name}}
        {%
          key = ((ann = ivar.annotation(HTTP::Param)) && ann[:key]) || (ivar.name.stringify.underscore[0..0] + ivar.name.stringify.camelcase[1..-1])
        %}

        var_key = key ? key + "[{{key.id}}]" : {{key}}

        {% scalar = ivar.type.annotation(Scalar) || (ivar.type.union? && ivar.type.union_types.all? { |t| t.annotation(Scalar) || t == Nil }) %}

        {% if converter = ivar.annotation(HTTP::Param) && ivar.annotation(HTTP::Param)[:converter] %}
          # `@{{ivar.name}}` has a `{{converter}}` as its converter
          {% if scalar %}
            # `{{ivar.type}}` is scalar, therefore calling
            # `{{converter}}#to_http_param` with common arguments
            {{converter}}.to_http_param(var, builder, var_key)
          {% else %}
            # `{{ivar.type}}` is not scalar, thus calling
            # `@{{ivar.name}}#to_http_param` with `converter:` argument
            var.to_http_param(builder, var_key, converter: {{converter}})
          {% end %}
        {% else %}
          # `@{{ivar.name}}` doesn't have a converter
          var.to_http_param(builder, var_key)
        {% end %}
      end
    {% end %}
  end

  # Serialalize `self` into an HTTP params query, returning a `String`.
  # Instance variables are by default seralized under camelCased keys,
  # unless explicitly specified with `@[HTTP::Param(key: "mykey")`.
  def to_query : String
    builder = HTTP::Params::Builder.new
    to_http_param(builder)
    builder.to_s
  end

  macro included
    # These methods are copied from `JSON::Serializable`
    #

    def self.from_http_param(query : String, path : Tuple)
      instance = allocate
      instance.initialize(_http_params_query: query, _http_params_path: path)
      GC.add_finalizer(instance) if instance.responds_to?(:finalize)
      instance
    end

    def self.from_query(query : String)
      from_http_param(query: query, path: Tuple.new)
    end

    macro inherited
      def self.from_http_param(query : String, path : Tuple)
        super
      end

      def self.from_query(query : String)
        super
      end
    end
  end

  # :nodoc:
  protected def initialize(*, _http_params_query : String, _http_params_path : Tuple)
    # There will be temp containers for actual param values
    {% for ivar in @type.instance_vars %}
      {{ivar.name}}_value = nil
    {% end %}

    # Non-scalar params may require multiple keys to be initialized.
    # For example, `foo[bar]=1&foo[baz]=2` will result in a *nested_queries* entry like this:
    # `nested_queries["foo"] == [{"bar", "1"}, {"baz", 2}]`
    nested_queries = Hash(String, Array(Tuple(String, String))).new

    _http_params_query.split('&').each do |param|
      begin
        key, value = param.split('=', 2)
      rescue IndexError
        # If there was no '=', just skip the param
        next
      end

      {% begin %}
        case key
        {% for ivar in @type.instance_vars %}
          # Scalar type is the one with `HTTP::Params::Serializable::Scalar` annotation,
          # or a union with all types either having this annotation or being `Nil`
          {%
            scalar = ivar.type.annotation(Scalar) || (ivar.type.union? && ivar.type.union_types.all? { |t| t.annotation(Scalar) || t == Nil })

            default_keys = [
              ivar.name.stringify,                                        # foo_bar
              ivar.name.stringify.underscore,                             # foo_bar
              ivar.name.stringify.camelcase,                              # FooBar
              ivar.name.underscore.gsub(/_/, "-"),                        # foo-bar
              ivar.name.underscore.split('_').map(&.camelcase).join('-'), # Foo-Bar
              ivar.name.stringify.underscore[0..0] +
              ivar.name.stringify.camelcase[1..-1], # fooBar
            ].uniq

            if (ann = ivar.annotation(HTTP::Param)) && ann[:key]
              keys = [ann[:key]]
            else
              keys = default_keys
            end
          %}

          # Explicit key match, e.g. `"foo"` or `"[foo]"`
          when {{keys.map(&.id.stringify.stringify).uniq.join(", ").id}}, {{keys.map { |k| "[#{k.id}]".stringify }.uniq.join(", ").id}}
            {% if scalar %}
              begin
                # FIXME: Dry with private macro. Currently ivar.type turns into Expressions
                # upon passing. See https://github.com/crystal-lang/crystal/issues/7191
                #
                # Check if the param has a converter annotated, e.g:
                #
                # ```
                # @[HTTP::Param(converter: Time::EpochConverter)]
                # getter time : Time
                # ```
                {% if converter = ivar.annotation(HTTP::Param) && ivar.annotation(HTTP::Param)[:converter] %}
                  # The param has a converter and it's scalar, so call the converter
                  {{ivar.name}}_value = {{converter}}.from_http_param(value)
                {% else %}
                  # The param doesn't have a converter, try to initialize
                  # its explicit type from the incoming value
                  {{ivar.name}}_value = {{ivar.type}}.from_http_param(value)
                {% end %}
              rescue TypeCastError
                unless value.empty?
                  raise ParamTypeCastError.new(_http_params_path + { {{ivar.name.stringify}} }, {{ivar.type}}, value.inspect)
                end

                # The container will be set to `nil` if the value is empty (`""`)
                {{ivar.name}}_value = nil
              end
            {% else %}
              # If `foo` is a complex object, then `foo=bar` makes no sense
              raise ExplicitKeyForNonScalarParam.new(_http_params_path + { {{ivar.name.stringify}} }, {{ivar.type}})
            {% end %}
          # Match on the key plus some nested (`"[..."`) content
          when {{keys.map { |k| "/^\\[?#{k.id}\\]?(?<nested>\\[.+)/" }.uniq.join(", ").id}}
            {% if scalar %}
              # If `foo` is a scalar, say, `Int32`, then `foo[something]=42` makes no sense
              raise NestedContentForScalarParamError.new(_http_params_path + { {{ivar.name.stringify}} }, HTTP::Params::Serializable.split_path($~["nested"]), {{ivar.type}})
            {% else %}
              # See the explaination of *nested_queries* above
              (nested_queries[{{ivar.name.stringify}}] ||= Array(Tuple(String, String)).new) << {$~["nested"], value}
            {% end %}
        {% end %}
        end
      {% end %}
    end

    nested_queries.each do |key, value|
      # Build a sub-query specially for given key, e.g.
      # `[{"bar", "1"}, {"baz", 2}]` turns into `bar=1&baz=2`
      query = value.join('&') { |(k, v)| "#{k}=#{v}" }

      {% begin %}
        case key
        {% for ivar in @type.instance_vars %}
          # Scalar type is the one with `HTTP::Params::Serializable::Scalar` annotation,
          # or a union with all types either having this annotation or being `Nil`
          {% scalar = ivar.type.annotation(Scalar) || (ivar.type.union? && ivar.type.union_types.all? { |t| t.annotation(Scalar) || t == Nil }) %}

          when {{ivar.name.stringify}}
            {% if scalar %}
              raise "BUG: Scalar type keys must not be added to nested_queries"
            {% else %}
              begin
                # FIXME: Dry with private macro. Currently ivar.type turns into Expressions
                # upon passing. See https://github.com/crystal-lang/crystal/issues/7191
                #
                # Check if the param has a converter annotated
                {% if converter = ivar.annotation(HTTP::Param) && ivar.annotation(HTTP::Param)[:converter] %}
                  # Initialize the type passing the `converter:` argument
                  {{ivar.name}}_value = {{ivar.type}}.from_http_param(
                    query,
                    _http_params_path + { {{ivar.name.stringify}} },
                    {{converter}},
                  )
                {% else %}
                  # The param doesn't have a converter, try to initialize
                  # its explicit type from the incoming value
                  {{ivar.name}}_value = {{ivar.type}}.from_http_param(
                    query,
                    _http_params_path + { {{ivar.name.stringify}} }
                  )
                {% end %}
              rescue TypeCastError
                raise ParamTypeCastError.new(_http_params_path + { {{ivar.name.stringify}} }, {{ivar.type}}, value.inspect)
              end
            {% end %}
        {% end %}
        end
      {% end %}
    end

    {% for ivar in @type.instance_vars %}
      {% if ivar.type.nilable? %}
        {% if ivar.has_default_value? %}
          @{{ivar.name}} = {{ivar.name}}_value || {{ivar.default_value}}
        {% else %}
          @{{ivar.name}} = {{ivar.name}}_value
        {% end %}
      {% else %}
        if {{ivar.name}}_value.nil?
          {% unless ivar.has_default_value? %}
            raise ParamMissingError.new(_http_params_path + { {{ivar.name.stringify}} })
          {% end %}
        else
          @{{ivar.name}} = {{ivar.name}}_value.not_nil!
        end
      {% end %}
    {% end %}
  end
end
