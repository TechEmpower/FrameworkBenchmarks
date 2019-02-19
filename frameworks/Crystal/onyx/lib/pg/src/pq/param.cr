module PQ
  ISO_8601 = "%FT%X.%L%z"
  # :nodoc:
  record Param, slice : Slice(UInt8), size : Int32, format : Int16 do
    delegate to_unsafe, to: slice

    #  Internal wrapper to represent an encoded parameter

    def self.encode(val : Nil)
      binary Pointer(UInt8).null.to_slice(0), -1
    end

    def self.encode(val : Slice)
      binary val, val.size
    end

    def self.encode(val : Array)
      text encode_array(val)
    end

    def self.encode(val : Time)
      text val.to_s(ISO_8601)
    end

    def self.encode(val : PG::Geo::Point)
      text "(#{val.x},#{val.y})"
    end

    def self.encode(val : PG::Geo::Line)
      text "{#{val.a},#{val.b},#{val.c}}"
    end

    def self.encode(val : PG::Geo::Circle)
      text "<(#{val.x},#{val.y}),#{val.radius}>"
    end

    def self.encode(val : PG::Geo::LineSegment)
      text "((#{val.x1},#{val.y1}),(#{val.x2},#{val.y2}))"
    end

    def self.encode(val : PG::Geo::Box)
      text "((#{val.x1},#{val.y1}),(#{val.x2},#{val.y2}))"
    end

    def self.encode(val : PG::Geo::Path)
      if val.closed?
        encode_points "(", val.points, ")"
      else
        encode_points "[", val.points, "]"
      end
    end

    def self.encode(val : PG::Geo::Polygon)
      encode_points "(", val.points, ")"
    end

    private def self.encode_points(left, points, right)
      string = String.build do |io|
        io << left
        points.each_with_index do |point, i|
          io << "," if i > 0
          io << "(" << point.x << "," << point.y << ")"
        end
        io << right
      end

      text string
    end

    def self.encode(val)
      text val.to_s
    end

    def self.binary(slice, size)
      new slice, size, 1_i16
    end

    def self.text(string : String)
      text string.to_slice
    end

    def self.text(slice : Bytes)
      new slice, slice.size, 0_i16
    end

    def self.encode_array(array)
      String.build do |io|
        encode_array(array, io)
      end
    end

    def self.encode_array(value, io)
      if value.is_a?(Array)
        io << "{"
        value.join(",", io) { |e| encode_array(e, io) }
        io << "}"
      else
        io << value
      end
    end
  end
end
