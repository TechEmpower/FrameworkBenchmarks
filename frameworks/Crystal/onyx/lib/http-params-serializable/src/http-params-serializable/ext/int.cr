{% for unsigned in [true, false] %}
  {% for bytes in %w(8 16 32 64) %}
    @[HTTP::Params::Serializable::Scalar]
    # :nodoc:
    struct {{"U".id if unsigned}}Int{{bytes.id}}
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
        return value.to_{{unsigned ? "u".id : "i".id}}{{bytes.id}}
      rescue ArgumentError
        raise TypeCastError.new
      end
    end
  {% end %}
{% end %}
