require "uri"
require "../annotations"

@[HTTP::Params::Serializable::Scalar]
class URI
  # Put `self` as an HTTP param into the *builder* at *key*.
  def to_http_param(builder : HTTP::Params::Builder, key : String)
    builder.add(key, to_http_param)
  end

  # Return `self` as an HTTP param string.
  def to_http_param
    to_s
  end

  # Parse `URI` from an HTTP param.
  def self.from_http_param(value : String)
    return URI.parse(value)
  end
end
