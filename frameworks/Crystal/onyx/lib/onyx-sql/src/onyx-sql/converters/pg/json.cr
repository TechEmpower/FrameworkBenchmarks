require "../pg"
require "../../ext/pg/result_set"
require "json"

# Converts between the PostgreSQL's native `JSON` and `JSONB` types and Crystal objects with
# `#to_json` and `.from_json` methods (e.g. `JSON::Serializable`).
# See `Field` to read about of how to apply converters.
#
# ```sql
# CREATE TABLE users (
#   meta  JSON NOT NULL DEFAULT "{}"
# );
# ```
#
# ```
# require "onyx-sql/converters/pg/json"
#
# class User
#   include Onyx::SQL::Model
#
#   struct Meta
#     include JSON::Serilalizable
#     property foo : String
#   end
#
#   schema do
#     type meta : Meta, converter: PG::JSON(Meta)
#   end
# end
# ```
module Onyx::SQL::Converters::PG::JSON(T)
  def self.to_db(value : T) : DB::Any
    value.to_json
  end

  def self.from_rs(rs : DB::ResultSet)
    bytes = rs.read_raw
    bytes.try do |bytes|
      T.from_json(String.new(bytes))
    end
  end
end
