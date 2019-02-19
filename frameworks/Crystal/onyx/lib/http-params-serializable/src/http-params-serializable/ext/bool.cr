@[HTTP::Params::Serializable::Scalar]
# :nodoc:
struct Bool
  # Put `self` as an HTTP param into the *builder* at *key*.
  def to_http_param(builder : HTTP::Params::Builder, key : String)
    builder.add(key, to_http_param)
  end

  # Return `self` as an HTTP param string.
  def to_http_param
    to_s
  end

  # Parse `self` from an HTTP param, returning `true` on `"true"` and `false` on `"false"`.
  def self.from_http_param(value : String)
    case value
    when "true"  then true
    when "false" then false
    else              raise TypeCastError.new
    end
  end
end
