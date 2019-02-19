require "json"

module PG
  alias PGValue = String | Nil | Bool | Int32 | Float32 | Float64 | Time | JSON::Any | PG::Numeric

  # :nodoc:
  module Decoders
    module Decoder
      abstract def decode(io, bytesize)

      def read(io, type)
        io.read_bytes(type, IO::ByteFormat::NetworkEndian)
      end

      def read_i16(io)
        read(io, Int16)
      end

      def read_i32(io)
        read(io, Int32)
      end

      def read_i64(io)
        read(io, Int64)
      end

      def read_u32(io)
        read(io, UInt32)
      end

      def read_u64(io)
        read(io, UInt64)
      end

      def read_f32(io)
        read(io, Float32)
      end

      def read_f64(io)
        read(io, Float64)
      end
    end

    struct StringDecoder
      include Decoder

      def decode(io, bytesize)
        String.new(bytesize) do |buffer|
          io.read_fully(Slice.new(buffer, bytesize))
          {bytesize, 0}
        end
      end
    end

    struct CharDecoder
      include Decoder

      def decode(io, bytesize)
        # TODO: can be done without creating an intermediate string
        String.new(bytesize) do |buffer|
          io.read_fully(Slice.new(buffer, bytesize))
          {bytesize, 0}
        end[0]
      end
    end

    struct BoolDecoder
      include Decoder

      def decode(io, bytesize)
        case byte = io.read_byte
        when 0
          false
        when 1
          true
        else
          raise "bad boolean decode: #{byte}"
        end
      end
    end

    struct Int16Decoder
      include Decoder

      def decode(io, bytesize)
        read_i16(io)
      end
    end

    struct Int32Decoder
      include Decoder

      def decode(io, bytesize)
        read_i32(io)
      end
    end

    struct Int64Decoder
      include Decoder

      def decode(io, bytesize)
        read_u64(io).to_i64
      end
    end

    struct UIntDecoder
      include Decoder

      def decode(io, bytesize)
        read_u32(io)
      end
    end

    struct Float32Decoder
      include Decoder

      def decode(io, bytesize)
        read_f32(io)
      end
    end

    struct Float64Decoder
      include Decoder

      def decode(io, bytesize)
        read_f64(io)
      end
    end

    struct PointDecoder
      include Decoder

      def decode(io, bytesize)
        Geo::Point.new(read_f64(io), read_f64(io))
      end
    end

    struct PathDecoder
      include Decoder

      def decode(io, bytesize)
        byte = io.read_byte.not_nil!
        closed = byte == 1_u8
        Geo::Path.new(PolygonDecoder.new.decode(io, bytesize - 1).points, closed)
      end
    end

    struct PolygonDecoder
      include Decoder

      def decode(io, bytesize)
        c = read_u32(io)
        count = (pointerof(c).as(Int32*)).value
        points = Array.new(count) do |i|
          PointDecoder.new.decode(io, 16)
        end
        Geo::Polygon.new(points)
      end
    end

    struct BoxDecoder
      include Decoder

      def decode(io, bytesize)
        x2, y2, x1, y1 = read_f64(io), read_f64(io), read_f64(io), read_f64(io)
        Geo::Box.new(x1, y1, x2, y2)
      end
    end

    struct LineSegmentDecoder
      include Decoder

      def decode(io, bytesize)
        Geo::LineSegment.new(read_f64(io), read_f64(io), read_f64(io), read_f64(io))
      end
    end

    struct LineDecoder
      include Decoder

      def decode(io, bytesize)
        Geo::Line.new(read_f64(io), read_f64(io), read_f64(io))
      end
    end

    struct CircleDecoder
      include Decoder

      def decode(io, bytesize)
        Geo::Circle.new(read_f64(io), read_f64(io), read_f64(io))
      end
    end

    struct JsonDecoder
      include Decoder

      def decode(io, bytesize)
        string = String.new(bytesize) do |buffer|
          io.read_fully(Slice.new(buffer, bytesize))
          {bytesize, 0}
        end
        JSON.parse(string)
      end
    end

    struct JsonbDecoder
      include Decoder

      def decode(io, bytesize)
        io.read_byte

        string = String.new(bytesize - 1) do |buffer|
          io.read_fully(Slice.new(buffer, bytesize - 1))
          {bytesize - 1, 0}
        end
        JSON.parse(string)
      end
    end

    JAN_1_2K = Time.new(2000, 1, 1, location: Time::Location::UTC)

    struct DateDecoder
      include Decoder

      def decode(io, bytesize)
        v = read_i32(io)
        JAN_1_2K + Time::Span.new(days: v, hours: 0, minutes: 0, seconds: 0)
      end
    end

    struct TimeDecoder
      include Decoder

      def decode(io, bytesize)
        v = read_i64(io) # microseconds
        sec, m = v.divmod(1_000_000)
        JAN_1_2K + Time::Span.new(seconds: sec, nanoseconds: m*1000)
      end
    end

    struct UuidDecoder
      include Decoder

      def decode(io, bytesize)
        bytes = uninitialized UInt8[6]

        String.new(36) do |buffer|
          buffer[8] = buffer[13] = buffer[18] = buffer[23] = 45_u8

          slice = bytes.to_slice[0, 4]

          io.read(slice)
          slice.hexstring(buffer + 0)

          slice = bytes.to_slice[0, 2]

          io.read(slice)
          slice.hexstring(buffer + 9)

          io.read(slice)
          slice.hexstring(buffer + 14)

          io.read(slice)
          slice.hexstring(buffer + 19)

          slice = bytes.to_slice
          io.read(slice)
          slice.hexstring(buffer + 24)

          {36, 36}
        end
      end
    end

    struct ByteaDecoder
      include Decoder

      def decode(io, bytesize)
        slice = Bytes.new(bytesize)
        io.read_fully(slice)
        slice
      end
    end

    struct NumericDecoder
      include Decoder

      def decode(io, bytesize)
        ndigits = read_i16(io)
        weight = read_i16(io)
        sign = read_i16(io)
        dscale = read_i16(io)
        digits = (0...ndigits).map { |i| read_i16(io) }
        PG::Numeric.new(ndigits, weight, sign, dscale, digits)
      end
    end

    @@decoders = Hash(Int32, PG::Decoders::Decoder).new(ByteaDecoder.new)

    def self.from_oid(oid)
      @@decoders[oid]
    end

    def self.register_decoder(decoder, oid)
      @@decoders[oid] = decoder
    end

    # https://github.com/postgres/postgres/blob/master/src/include/catalog/pg_type.h
    register_decoder BoolDecoder.new, 16         # bool
    register_decoder ByteaDecoder.new, 17        # bytea
    register_decoder CharDecoder.new, 18         # "char" (internal type)
    register_decoder StringDecoder.new, 19       # name (internal type)
    register_decoder Int64Decoder.new, 20        # int8 (bigint)
    register_decoder Int16Decoder.new, 21        # int2 (smallint)
    register_decoder Int32Decoder.new, 23        # int4 (integer)
    register_decoder StringDecoder.new, 25       # text
    register_decoder UIntDecoder.new, 26         # oid (internal type)
    register_decoder JsonDecoder.new, 114        # json
    register_decoder StringDecoder.new, 142      # xml
    register_decoder JsonbDecoder.new, 3802      # jsonb
    register_decoder Float32Decoder.new, 700     # float4
    register_decoder Float64Decoder.new, 701     # float8
    register_decoder StringDecoder.new, 705      # unknown
    register_decoder StringDecoder.new, 1042     # blchar
    register_decoder StringDecoder.new, 1043     # varchar
    register_decoder DateDecoder.new, 1082       # date
    register_decoder TimeDecoder.new, 1114       # timestamp
    register_decoder NumericDecoder.new, 1700    # numeric
    register_decoder TimeDecoder.new, 1184       # timestamptz
    register_decoder Int32Decoder.new, 2206      # regtype
    register_decoder UuidDecoder.new, 2950       # uuid
    register_decoder PointDecoder.new, 600       # point
    register_decoder LineSegmentDecoder.new, 601 # lseg
    register_decoder PathDecoder.new, 602        # path
    register_decoder BoxDecoder.new, 603         # box
    register_decoder PolygonDecoder.new, 604     # polygon
    register_decoder LineDecoder.new, 628        # line
    register_decoder CircleDecoder.new, 718      # circle
  end
end

require "./decoders/*"
