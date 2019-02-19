require "../sqlite3"
require "uuid"

# Converts between the SQLite3 `BLOB` type and Crystal's `UUID`.
# See `Field` to read about of how to apply converters.
#
# See the comparison of uuid converters at `Converters::SQLite3::UUIDBlob`.
#
# ```sql
# CREATE TABLE users (
#   uuid  TEXT,
#   uuids TEXT
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3/uuid_text"
#
# class User
#   include Onyx::SQL::Model
#
#   schema do
#     pkey uuid : UUID = UUID.random, converter: SQLite3::UUIDText
#     type uuids : Array(UUID), converter: SQLite3::UUIDText
#   end
# end
# ```
module Onyx::SQL::Converters::SQLite3::UUIDText
  def self.to_db(value : ::UUID) : DB::Any
    value.to_s
  end

  def self.to_db(values : Enumerable(::UUID)) : DB::Any
    Any(String).to_db(values.map(&.to_s))
  end

  def self.from_rs(rs) : ::UUID?
    rs.read(String | Nil).try { |s| ::UUID.new(s) }
  end

  def self.from_rs_array(rs) : ::Array(::UUID)?
    Any(String).from_rs_array(rs).try &.map { |s| ::UUID.new(s) }
  end
end
