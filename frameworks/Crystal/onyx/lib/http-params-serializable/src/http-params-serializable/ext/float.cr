{% for bytes in %w(32 64) %}
  @[HTTP::Params::Serializable::Scalar]
  # :nodoc:
  struct Float{{bytes.id}}
    # Put `self` as an HTTP param into the *builder* at *key*.
    def to_http_param(builder : HTTP::Params::Builder, key : String)
      builder.add(key, to_http_param)
    end

    # Return `self` as an HTTP param string.
    def to_http_param
      to_s
    end

    # Parse `self` from an HTTP param.
    def self.from_http_param(value : String)
      return value.to_f{{bytes.id}}
    rescue ArgumentError
      raise TypeCastError.new
    end
  end
{% end %}
