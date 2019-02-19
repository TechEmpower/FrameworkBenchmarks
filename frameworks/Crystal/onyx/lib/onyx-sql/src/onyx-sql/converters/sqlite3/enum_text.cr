require "../sqlite3"

# Converts between the SQLite3 `TEXT` type and Crystal `Enum`s.
# See `Field` to read about of how to apply converters.
#
# See the comparison of enum converters at `Converters::SQLite3::EnumInt`.
#
# ```sql
# CREATE TABLE users (
#   permission  TEXT  NOT NULL  DEFAULT 'create_posts',
#   permissions TEXT  NOT NULL  DEFAULT '{create_posts,edit_posts}'
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3"
#
# class User
#   include Onyx::SQL::Model
#
#   enum Permission
#     CreatePosts
#     EditPosts
#   end
#
#   schema do
#     type permission : Permission, converter: SQLite3::EnumText(User::Permission))]
#     type permissions : Array(Permission), converter: SQLite3::EnumText(User::Permission)
#   end
# end
# ```
module Onyx::SQL::Converters::SQLite3::EnumText(T)
  def self.to_db(value : T)
    value.to_s
  end

  def self.to_db(values : Enumerable(T))
    Any(String).to_db(values.map(&.to_s))
  end

  def self.from_rs(rs) : T?
    rs.read(String | Nil).try { |s| T.parse(s) }
  end

  def self.from_rs_array(rs) : ::Array(T)?
    Any(String).from_rs_array(rs).try &.map { |s| T.parse(s) }
  end
end
