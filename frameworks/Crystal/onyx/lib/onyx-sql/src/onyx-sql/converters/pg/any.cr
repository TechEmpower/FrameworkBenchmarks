require "../pg"
require "pg/pq/param"

# Converts between the PostgreSQL values (`Nil`, `Bytes`, `Number`, `Bool`, `String`, `Array`,
# `Time` and also [`PG::Geo`](https://github.com/will/crystal-pg/blob/master/src/pg/geo.cr))
# and native Crystal types. It automatically handles enumerables.
# See `Field` to read about of how to apply converters.
#
# NOTE: Although PostgreSQL natively supports arrays, some databases don't (for example, SQLite).
# That's why you have to explicitly declare a converter on a variable which is (or may be) an array.
#
# ```sql
# CREATE TABLE users (
#   id              INTEGER,
#   favorite_movies TEXT[]
# );
# ```
#
# ```
# require "onyx-sql/converters/pg"
#
# class User
#   include Onyx::SQL::Model
#
#   schema do
#     type id : Int32, converter: PG::Any(Int32)
#     type favorite_movies : Array(String), converter: PG::Any(String)
#   end
# end
#
# Onyx::SQL::Converters::PG::Any(Int32).new.to_db([42, 43]) # => Bytes
# ```
module Onyx::SQL::Converters::PG::Any(T)
  protected def self.to_db(*, x : U) : DB::Any forall U
    {% unless [Nil, Bytes, Number, Bool, String, Array, Time, ::PG::Geo::Point, ::PG::Geo::Line, ::PG::Geo::Circle, ::PG::Geo::LineSegment, ::PG::Geo::Box, ::PG::Geo::Path, ::PG::Geo::Polygon].any? { |t| U <= t } %}
      {% raise "Type #{U} is not supported by #{@type}. Consider using another converter" %}
    {% end %}

    param = PQ::Param.encode(x)

    case param.format
    when 0_i16 then String.new(param.slice)
    when 1_i16 then param.slice
    else
      "BUG: Unknown PQ::Param format #{param.format}"
    end
  end

  def self.to_db(value : T) : DB::Any
    to_db(x: value)
  end

  def self.to_db(values : Enumerable(T)) : DB::Any
    to_db(x: values.to_a)
  end

  def self.from_rs(rs : DB::ResultSet) : T?
    rs.read(T | Nil)
  end

  def self.from_rs_array(rs : DB::ResultSet) : Array(T)?
    rs.read(Array(T) | Nil)
  end
end
