require "../pg"
require "uuid"

# Converts between the PostgreSQL's `UUID` type and Crystal's `UUID`.
# See `Field` to read about of how to apply converters.
#
# ```sql
# CREATE EXTENSION pgcrypto;
# CREATE TABLE users (
#   uuid  UUID PRIMARY KEY  DEFAULT gen_random_uuid(),
# );
# ```
#
# ```
# require "onyx-sql/converters/pg/uuid"
#
# class User
#   include Onyx::SQL::Model
#
#   schema do
#     pkey uuid : UUID, converter: PG::UUID
#   end
# end
# ```
module Onyx::SQL::Converters::PG::UUID
  def self.to_db(value : ::UUID) : DB::Any
    value.to_s
  end

  def self.to_db(values : Enumerable(::UUID)) : DB::Any
    values.map { |v| to_db(v) }.join(',').try { |x| "{#{x}}" }
  end

  def self.from_rs(rs) : ::UUID?
    rs.read(String | Nil).try { |s| ::UUID.new(s) }
  end

  def self.from_rs_array(rs) : ::Array(::UUID)?
    rs.read(::Array(String) | Nil).try &.map { |s| ::UUID.new(s) }
  end
end
