# :nodoc:
class Array(T)
  # Serialalize `self` into an HTTP params query with the *builder* at *key*,
  # optionally convering underlying elements with *converter*.
  def to_http_param(builder : HTTP::Params::Builder, key : String? = nil, converter : C = nil) : Nil forall C
    {% begin %}
      {% scalar = T.annotation(HTTP::Params::Serializable::Scalar) || (T.union? && T.union_types.all? { |t| t.annotation(HTTP::Params::Serializable::Scalar) || t == Nil }) %}

      {% if scalar %}
        e_key = key ? key + "[]" : "[]"
      {% end %}

      each_with_index do |elem, index|
        if e = elem
          {% unless scalar %}
            e_key = key ? key + "[#{index}]" : "[#{index}]"
          {% end %}

          # FIXME: For some reason `Time::EpochConverter`
          # turns into `Time::EpochConverter:Module`
          {% if C != Nil && (converter = C.name.gsub(/:Module$/, "").id) %}
            {% if scalar %}
              {{converter}}.to_http_param(e, builder, e_key)
            {% else %}
              e.to_http_param(builder, e_key, converter: {{converter}})
            {% end %}
          {% else %}
            e.to_http_param(builder, e_key)
          {% end %}
        end
      end
    {% end %}
  end

  # Serialalize `self` into an HTTP params query, returning a `String`.
  def to_http_param
    builder = HTTP::Params::Builder.new
    to_http_param(builder)
    builder.to_s
  end

  # Initialize `self` from an HTTP param query,
  # optionally convering underlying elements with *converter*.
  # *path* is used to raise convenient errors.
  def self.from_http_param(query : String, path : Tuple, converter : C = nil) : self forall C
    {% begin %}
      {% scalar = T.annotation(HTTP::Params::Serializable::Scalar) || (T.union? && T.union_types.all? { |t| t.annotation(HTTP::Params::Serializable::Scalar) || t == Nil }) %}

      instance = self.new
      nested_queries = Hash(Int32, Array(Tuple(String, String))).new

      query.split('&').each do |param|
        begin
          key, value = param.split('=')
        rescue e : IndexError
          next
        end

        case key
        when "[]"
          {% if scalar %}
            new_path = path + {""}

            begin
              instance << parse(T, {{C.stringify}}, value, new_path)
            rescue ex : TypeCastError
              {% if T.nilable? %}
                raise HTTP::Params::Serializable::ParamTypeCastError.new(new_path, T, value.inspect) unless value.empty?
                instance << nil
              {% else %}
                raise HTTP::Params::Serializable::ParamTypeCastError.new(new_path, T, value.inspect)
              {% end %}
            end
          {% else %}
            raise HTTP::Params::Serializable::EmptyIndexForNonScalarArrayParamError.new(path, [""])
          {% end %}
        when /^\[(?<index>.*)\](?<nested>\[.+)/
          {% if scalar %}
            raise HTTP::Params::Serializable::NestedContentForScalarParamError.new(path, [$~["index"]] + HTTP::Params::Serializable.split_path($~["nested"]), {{@type}})
          {% else %}
            if $~["index"].empty?
              raise HTTP::Params::Serializable::EmptyIndexForNonScalarArrayParamError.new(path, [$~["index"]] + HTTP::Params::Serializable.split_path($~["nested"]))
            else
              begin
                index = $~["index"].to_i32
              rescue ArgumentError
                raise HTTP::Params::Serializable::InvalidParamIndexError.new(path + {$~["index"]}, $~["index"].inspect)
              end

              (nested_queries[index] ||= Array(Tuple(String, String)).new) << {$~["nested"], value}
            end
          {% end %}
        when /^\[(?<index>.+)\]$/
          begin
            index = $~["index"].to_i32
          rescue ArgumentError
            raise HTTP::Params::Serializable::InvalidParamIndexError.new(path + {$~["index"]}, $~["index"].inspect)
          end

          new_path = path + {$~["index"]}

          {% if scalar %}
            begin
              parsed = parse(T, {{C.stringify}}, value, new_path)
            rescue ex : TypeCastError
              {% if T.nilable? %}
                raise HTTP::Params::Serializable::ParamTypeCastError.new(new_path, T, value.inspect) unless value.empty?
                parsed = nil
              {% else %}
                raise HTTP::Params::Serializable::ParamTypeCastError.new(new_path, T, value.inspect)
              {% end %}
            end

            if index < 0
              raise HTTP::Params::Serializable::ParamIndexOutOfBoundsError.new(new_path, index, instance.size)
            elsif index < instance.size
              instance[index] = parsed
            elsif index == instance.size
              instance << parsed
            else
              {% if T == Nil || (T.union? && T.union_types.includes?(Nil)) %}
                (index - instance.size).times do
                  instance << nil
                end

                instance << parsed
              {% else %}
                raise HTTP::Params::Serializable::ParamIndexOutOfBoundsError.new(new_path, index, instance.size)
              {% end %}
            end
          {% else %}
            raise HTTP::Params::Serializable::ExplicitKeyForNonScalarParam.new(new_path, T)
          {% end %}
        end
      end

      nested_queries.keys.sort.each do |index|
        new_path = path + {index.to_s}
        query = nested_queries[index].join('&') { |(k, v)| "#{k}=#{v}" }

        begin
          parsed = parse(T, {{C.stringify}}, query, new_path)
        rescue ex : TypeCastError
          # The type is guaranteed to be non-scalar, thus expected to rescue TypeCastError
          raise "BUG: Non-scalar type #{T} is expected to rescue TypeCastError"
        end

        if index < instance.size
          instance[index] = parsed
        elsif index == instance.size
          instance << parsed
        else
          {% if T == Nil || (T.union? && T.union_types.includes?(Nil)) %}
            (index - instance.size).times do
              instance << nil
            end

            instance << parsed
          {% else %}
            raise HTTP::Params::Serializable::ParamIndexOutOfBoundsError.new(new_path, index, instance.size)
          {% end %}
        end
      end

      instance
    {% end %}
  end

  private macro parse(type, converter, value, path)
    {% scalar = type.resolve.annotation(HTTP::Params::Serializable::Scalar) || (type.resolve.union? && type.resolve.union_types.all? { |t| t.annotation(HTTP::Params::Serializable::Scalar) || t == Nil }) %}

    # FIXME: For some reason `Time::EpochConverter`
    # turns into `Time::EpochConverter:Module`
    {% if converter != "Nil" && (converter = converter.gsub(/:Module$/, "").id) %}
      # If the underlying type is an Array or (Array | Nil) itself,
      # pass the converter further
      {% if scalar %}
        # Explicitly call the converter, waiting for the correct return type
        {{converter}}.from_http_param({{value}})
      {% else %}
        # Initialize the array then, passing the `converter:` argument
        {{type.resolve}}.from_http_param(
          {{value}},
          {{path}},
          {{converter}},
        )
      {% end %}
    {% else %}
      # The param doesn't have a converter, try to initialize
      # its explicit type from the incoming value
      #

      {% if scalar %}
        {{type.resolve}}.from_http_param({{value}})
      {% else %}
        {{type.resolve}}.from_http_param({{value}}, {{path}})
      {% end %}
    {% end %}
  end
end
