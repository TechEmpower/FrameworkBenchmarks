require "../sqlite3"
require "json"

# Converts between the SQLite3 `TEXT` type and Crystal objects with
# `#to_json` and `.from_json` methods (e.g. `JSON::Serializable`).
# See `Field` to read about of how to apply converters.
#
# ```sql
# CREATE TABLE users (
#   meta  TEXT  NOT NULL  DEFAULT '{"foo":"bar"}'
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3/json"
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
#     type meta : Meta, converter: SQLite3::JSON(Meta)
#   end
# end
# ```
module Onyx::SQL::Converters::SQLite3::JSON(T)
  def self.to_db(value : T)
    value.to_json
  end

  def self.to_db(values : Enumerable(T))
    {% raise "Not implemented" %}
  end

  def self.from_rs(rs) : T?
    rs.read(String | Nil).try { |json| T.from_json(json) }
  end

  def self.from_rs_array(rs) : ::Array(T)?
    {% raise "Not implemented" %}
  end
end
