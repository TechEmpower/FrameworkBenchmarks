@[HTTP::Params::Serializable::Scalar]
# :nodoc:
struct Char
  # Put `self` as an HTTP param into the *builder* at *key*.
  def to_http_param(builder : HTTP::Params::Builder, key : String)
    builder.add(key, to_http_param)
  end

  # Return `self` as an HTTP param string.
  def to_http_param
    self.to_s
  end

  # Parse `self` from an HTTP param. Raises `TypeCastError` if the value length != 1.
  def self.from_http_param(value : String)
    chars = value.chars
    raise TypeCastError.new if chars.size != 1
    return chars.first
  end
end
