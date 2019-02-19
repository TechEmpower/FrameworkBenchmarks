require "../sqlite3"

# Converts between the SQLite3 `INTEGER` (or `TEXT` for arrays) type and Crystal `Enum`s.
# See `Field` to read about of how to apply converters.
#
# Comparison of `Converters::SQLite3::EnumInt` and `Converters::SQLite3::EnumText`:
#
# ```text
# |                       | EnumInt | EnumText |
# | --------------------- | ------- | -------- |
# | Bytes needed to store | Less    | More     |
# | Depends on enum index | Yes     | No       |
# ```
#
# In a nutshell, if you modify a `Enum` values in the further cycles of application development,
# it will be harder to migrate changes in the database if you use `EnumInt` converter, but it takes less bytes.
#
# ```sql
# CREATE TABLE users (
#   role  INTEGER NOT NULL  DEFAULT 0,
#   roles TEXT    NOT NULL  DEFAULT '{0}'
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3"
#
# class User
#   include Onyx::SQL::Model
#
#   enum Role
#     Writer
#     Moderator
#     Admin
#   end
#
#   schema do
#     type role : Role, converter: SQLite3::EnumInt(User::Role)
#     type roles : Array(Role), converter: SQLite3::EnumInt(User::Role)
#   end
# end
# ```
module Onyx::SQL::Converters::SQLite3::EnumInt(T)
  def self.to_db(value : T)
    value.to_i
  end

  def self.to_db(values : Enumerable(T))
    Any(Int32).to_db(values.map(&.to_i))
  end

  def self.from_rs(rs) : T?
    rs.read(Int32 | Nil).try { |i| T.new(i) }
  end

  def self.from_rs_array(rs) : ::Array(T)?
    Any(Int32).from_rs_array(rs).try &.map { |i| T.new(i) }
  end
end
