module HTTP
  # An HTTP parameter annotation.
  #
  # Options:
  #
  # * *key* -- a key to use for (de)serializing.  By default all possible variations
  # of the instance variable name are used (i.e. `"foo_bar"`, `"fooBar"`, `"FooBar"`,
  # `"foo-bar"` and `"Foo-Bar"` for a `@foo_bar` variable)
  # * *converter* -- a converter to use for casting.
  # Some types implement converter forwarding, for example, `Array`:
  #
  # ```
  # require "http-params-parseable"
  # require "http-params-parseable/ext/time/epoch_converter"
  #
  # struct MyParams
  #   include HTTP::Params::Serializable
  #
  #   @[HTTP::Param(converter: Time::EpochConverter, key: "theTime")]
  #   getter time : Array(Time)
  # end
  #
  # params = MyParams.from_query("theTime[]=1544958806")
  # pp params.time.class # => Array(Time)
  # ```
  annotation Param
  end

  struct Params
    module Serializable
      # Annotate a type to mark it as a scalar, so a single string query value will be tried to cast.
      # For example, `Int32` is scalar by default, thus in case `"foo=42"`, `"42"` is tried to cast to `Int32`.
      #
      # Good candidates to become scalar: `URI` and `Time`; for example:
      #
      # ```
      # @[HTTP::Params::Serializable::Scalar]
      # class URI
      #   def to_http_param(builder : HTTP::Params::Builder, key : String)
      #     builder.add(key, to_s)
      #   end
      #
      #   def self.from_http_param(value : String)
      #     return URI.parse(value)
      #   end
      # end
      #
      # struct MyParams
      #   include HTTP::Params::Serializable
      #   getter uri : URI
      # end
      #
      # params = MyParams.from_query("uri=https://example.com")
      # pp params.uri # => <URI @host="example.com" ...>
      # ```
      #
      # `URI` and `Time::EpochConverter` extensions are shipped by default:
      #
      # ```
      # require "http-params-parseable"
      # require "http-params-parseable/ext/uri"
      # require "http-params-parseable/ext/time/epoch_converter"
      #
      # struct MyParams
      #   include HTTP::Params::Serializable
      #
      #   getter uri : URI
      #
      #   @[HTTP::Param(converter: Time::EpochConverter)]
      #   getter time : Time
      # end
      # ```
      #
      # If you have annotated a type as scalar, then it must implement these methods:
      #
      # ```
      # def to_http_param(builder : HTTP::Params::Builder, key : String)
      #   # E.g. builder.add(key, self.to_s)
      # end
      #
      # def self.from_http_param(value : String) : self
      #   # E.g. value.to_s
      # end
      # ```
      #
      # Otherwise (if the type is not `Scalar`), the methods become a little more complex:
      #
      # ```
      # def to_http_param(builder : HTTP::Params::Builder, key : String? = nil)
      #   # Notice the *key* is nilable
      # end
      #
      # # Or if you're using a converter on a param:
      # def to_http_param(builder : HTTP::Params::Builder, key : String? = nil, converter : C = nil) forall C
      #   # ditto
      # end
      #
      # def self.from_http_param(query : String, path : Tuple) : self
      #   # Notice the *path* argument
      # end
      #
      # # Or if you're using a converter on a param:
      # def self.from_http_param(query : String, path : Tuple, converter : C = nil) : self forall C
      #   # ditto
      # end
      # ```
      annotation Scalar
      end
    end
  end
end
