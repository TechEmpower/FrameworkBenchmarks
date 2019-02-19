struct HTTP::Params
  module Serializable
    # A base class for all `HTTP::Params::Serializable` errors.
    class Error < Exception
      # The param query path. Can be used to display to clients.
      getter path : Array(String) = Array(String).new

      def build_path(*path)
        HTTP::Params::Serializable.build_path(*path)
      end
    end

    # Raised when a param is not nilable and missing from the query.
    class ParamMissingError < Error
      def initialize(path : Tuple)
        @path = path.to_a.map(&.as(String))

        super("Parameter #{build_path(*path).inspect} is missing")
      end
    end

    # Raised when a param cannot be casted to the desired type.
    class ParamTypeCastError < Error
      # The desired type which the param failed to be cast to.
      getter type : String

      def initialize(path : Tuple, type, value)
        @path = path.to_a.map(&.as(String))
        @type = type.to_s

        super("Parameter #{build_path(*path).inspect} cannot be cast from #{value} to #{type}")
      end
    end

    # Raised when a param in the query has an invalid index (e.g. string value).
    # For example, if `foo` is `Array(Int32)`, then this query would raise: `"foo[bar]=42"`.
    class InvalidParamIndexError < Error
      def initialize(path, value)
        @path = path.to_a

        super("Parameter #{build_path(*path).inspect} has invalid index #{value}")
      end
    end

    # Raised when a param in the query has an index out of boundaries.
    # For example, if `foo` is `Array(Int32)`, then this query would raise: `"foo[-1]=42"`.
    class ParamIndexOutOfBoundsError < Error
      def initialize(path : Tuple, index, size)
        @path = path.to_a

        super("Parameter #{build_path(*path).inspect} has index #{index} out of bounds (current size is #{size})")
      end
    end

    # Raised when a non-scalar type has a empty (`"[]"`) index in the query.
    # Complex (i.e. non-scalar) objects require explicit numeration for their instances, e.g.
    # if `foo` is `Array(NestedParams(a : Int32, b : Int32))`, then `foo[][a]=1&foo[][b]=2`
    # makes no sense, because it's not clear whether is it the same object.
    class EmptyIndexForNonScalarArrayParamError < Error
      def initialize(param_path : Tuple, nested_path : Array(String))
        @path = param_path.to_a + [""]
        full_path = param_path.to_a + nested_path

        super("Parameter #{build_path(*param_path).inspect} is a non-scalar array, but has empty index at #{(build_path(full_path) + '=').inspect}")
      end
    end

    # Raised when a scalar param has nested content in the query.
    # For example, if `foo` is `Int32`, then `foo[bar]=` makes no sense.
    class NestedContentForScalarParamError < Error
      def initialize(param_path : Tuple, nested_path : Array(String), type)
        @path = param_path.to_a + nested_path
        full_path = @path
        _type = type < Array ? "Array" : type < Hash ? "Hash" : type

        super("Parameter #{build_path(*param_path).inspect} is of type #{_type}, but has nested content at #{build_path(full_path).inspect}")
      end
    end

    # Raised when a non-scalar param is tried to be set explicitly with a single key.
    # For example, if `foo` is a complext object, then `foo=bar` makes no sense.
    class ExplicitKeyForNonScalarParam < Error
      def initialize(path : Tuple, type)
        @path = path.to_a
        _type = type < Array ? "Array" : type < Hash ? "Hash" : "Object"

        super("Parameter #{build_path(*path).inspect} is of type #{_type}, but set explicitly with #{(build_path(*path) + '=').inspect}")
      end
    end
  end
end
