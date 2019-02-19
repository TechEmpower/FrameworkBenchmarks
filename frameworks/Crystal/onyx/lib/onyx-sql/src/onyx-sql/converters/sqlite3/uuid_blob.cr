require "../sqlite3"
require "uuid"

# Converts between the SQLite3 `BLOB` type and Crystal's `UUID`.
# See `Field` to read about of how to apply converters.
#
# Comparison of `Converters::SQLite3::UUIDBlob` and `Converters::SQLite3::UUIDText`:
#
# ```text
# |                       | UUIDBlob | UUIDText |
# | --------------------- | -------- | -------- |
# | Bytes needed to store | 16       | ~32      |
# | Comparable            | No       | Yes      |
# | Can store array       | No       | Yes      |
# ```
#
# When you use `BLOB` type for storing uuids, you cannot compare them with `WHERE uuid = ?`
# (making it useless as primary keys) and cannot store an array of uuids in a single column,
# but it occupies less bytes.
#
# ```sql
# CREATE TABLE users (
#   uuid  BLOB
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3/uuid_blob"
#
# class User
#   include Onyx::SQL::Model
#
#   schema do
#     type uuid : UUID = UUID.random, converter: SQLite3::UUIDBlob
#   end
# end
# ```
module Onyx::SQL::Converters::SQLite3::UUIDBlob
  def self.to_db(value : ::UUID) : DB::Any
    value.to_slice
  end

  def self.to_db(values : Enumerable(::UUID)) : DB::Any
    {% raise "Not implemented" %}
  end

  def self.from_rs(rs) : ::UUID?
    rs.read(Bytes | Nil).try { |b| ::UUID.new(b) }
  end

  def self.from_rs_array(rs) : ::Array(::UUID)?
    {% raise "Not implemented" %}
  end
end
