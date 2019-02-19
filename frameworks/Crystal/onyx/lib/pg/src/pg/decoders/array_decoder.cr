module PG
  module Decoders
    # Generic Array decoder: decodes to a recursive array type
    struct ArrayDecoder(T, D)
      include Decoder

      def decode(io, bytesize)
        dimensions, dim_info = Decoders.decode_array_header(io)

        if dimensions == 0
          ([] of T).as(T)
        elsif dimensions == 1 && dim_info.first[:lbound] == 1
          # allow casting down to unnested crystal arrays
          build_simple_array(io, dim_info.first[:dim]).as(T)
        else
          if dim_info.any? { |di| di[:lbound] < 1 }
            raise PG::RuntimeError.new("Only lower-bounds >= 1 are supported")
          end

          # recursively build nested array
          get_element(io, dim_info).as(T)
        end
      end

      def build_simple_array(io, size)
        Array(T).new(size) { get_next(io) }
      end

      def get_element(io, dim_info)
        if dim_info.size == 1
          lbound = dim_info.first[:lbound] - 1 # in lower-bound is not 1
          Array(T).new(dim_info.first[:dim] + lbound) do |i|
            i < lbound ? nil : get_next(io)
          end
        else
          Array(T).new(dim_info.first[:dim]) do |i|
            get_element(io, dim_info[1..-1])
          end
        end
      end

      def get_next(io)
        bytesize = read_i32(io)
        if bytesize == -1
          nil
        else
          D.new.decode(io, bytesize)
        end
      end
    end

    # Specific array decoder method: decodes to exactly Array(T).
    # Used when invoking, for example `rs.read(Array(Int32))`.
    def self.decode_array(io, bytesize, t : Array(T).class) forall T
      dimensions, dim_info = decode_array_header(io)
      if dimensions == 0
        return [] of T
      end

      decode_array_element(io, t, dim_info)
    end

    def self.decode_array_element(io, t : Array(T).class, dim_info) forall T
      size = dim_info.first[:dim]
      rest = dim_info[1..-1]
      Array(T).new(size) { decode_array_element(io, T, rest) }
    end

    {% for type in %w(Bool Char Int16 Int32 String Int64 Float32 Float64) %}
      def self.decode_array_element(io, t : {{type.id}}.class, dim_info)
        bytesize = read_i32(io)
        if bytesize == -1
          raise PG::RuntimeError.new("unexpected NULL")
        else
          {{type.id}}Decoder.new.decode(io, bytesize)
        end
      end

      def self.decode_array_element(io, t : {{type.id}}?.class, dim_info)
        bytesize = read_i32(io)
        if bytesize == -1
          nil
        else
          {{type.id}}Decoder.new.decode(io, bytesize)
        end
      end
    {% end %}

    def self.decode_array_header(io)
      dimensions = read_i32(io)
      has_null = read_i32(io) == 1 # unused
      oid = read_i32(io)           # unused but in header
      dim_info = Array({dim: Int32, lbound: Int32}).new(dimensions) do |i|
        {
          dim:    read_i32(io),
          lbound: read_i32(io),
        }
      end
      {dimensions, dim_info}
    end

    def self.read_i32(io)
      io.read_bytes(Int32, IO::ByteFormat::NetworkEndian)
    end
  end

  macro array_type(oid, t)
    alias {{t}}Array = {{t}}? | Array({{t}}Array)
    module Decoders
      register_decoder ArrayDecoder({{t}}Array, {{t}}Decoder).new, {{oid}}
    end
  end

  array_type 1000, Bool
  array_type 1002, Char
  array_type 1005, Int16
  array_type 1007, Int32
  array_type 1009, String
  array_type 1016, Int64
  array_type 1021, Float32
  array_type 1022, Float64
end
