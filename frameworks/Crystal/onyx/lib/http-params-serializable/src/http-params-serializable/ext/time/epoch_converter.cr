require "../../annotations"

@[HTTP::Params::Serializable::Scalar]
struct Time
  module EpochConverter
    # Put *value* as a unix timestamp into the *builder* at *key*.
    def self.to_http_param(value : Time, builder : HTTP::Params::Builder, key : String)
      builder.add(key, to_http_param(value))
    end

    # Return *value* as a unix timestamp string.
    def self.to_http_param(value : Time)
      value.to_unix.to_s
    end

    # Parse `Time` from an HTTP param as unix timestamp.
    def self.from_http_param(value : String) : Time
      return Time.unix(value.to_i64)
    rescue ArgumentError
      raise TypeCastError.new
    end
  end
end
