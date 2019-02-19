require "../pg"
require "../../ext/pg/result_set"

# Converts between the PostgreSQL `ENUM` type and Crystal `Enum`s.
# See `Field` to read about of how to apply converters.
#
# ```sql
# CREATE TYPE users_role AS ENUM ('writer', 'moderator', 'admin');
# CREATE TYPE users_permissions AS ENUM ('create_posts', 'edit_posts');
#
# CREATE TABLE users (
#   role        users_role          NOT NULL  DEFAULT 'writer',
#   permissions users_permissions[] NOT NULL  DEFAULT '{create_posts}'
# );
# ```
#
# ```
# require "onyx-sql/converters/pg"
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
#   enum Permission
#     CreatePosts
#     EditPosts
#   end
#
#   schema do
#     type role : Role, converter: PG::Enum(User::Role)
#     type permissions : Array(Permission), converter: PG::Enum(User::Permission)
#   end
# end
# ```
module Onyx::SQL::Converters::PG::Enum(T)
  def self.to_db(value : T) : DB::Any
    value.to_s.underscore
  end

  def self.to_db(values : Enumerable(T)) : DB::Any
    values.map { |v| to_db(v) }.join(',').try { |x| "{#{x}}" }
  end

  def self.from_rs(rs : DB::ResultSet) : T?
    rs.read(Bytes | Nil).try { |bytes| T.parse(String.new(bytes)) }
  end

  def self.from_rs_array(rs : DB::ResultSet) : Array(T)?
    rs.read_raw.try do |bytes|
      String.new(bytes).delete("^a-z_\n").split("\n").map do |s|
        T.parse(s)
      end
    end
  end
end
