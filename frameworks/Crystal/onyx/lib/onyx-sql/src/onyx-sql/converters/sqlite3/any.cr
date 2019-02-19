require "../sqlite3"

# Converts between the SQLite3 types (`NULL`, `INTEGER`, `REAL`, `TEXT` and `BLOB`)
# and native Crystal types.
# See `Field` to read about of how to apply converters.
#
# SQLite3 does **not** natively support neither arrays nor `Time` nor `Bool`. This converters
# implements the following workarounds:
#
# * `Time` is stored as string in the following format: `"%F %H:%M:%S.%L"`.
# For example, `"2019-02-13 22:58:28.942"`.
# * `Bool`s are stored as `"1"` and `"0"`
# * Enumerable types are stored as `"{x,y}"` strings. For example, `[42, 43]` will be stored as
# `"{42,43"`, `[Time]` as `"{2019-02-13 14:17:59.228}"` and empty arrays as `"{}"`
#
# As this converter is two-way, the implications above apply to values set by the developer as well.
# For example, if a bool value is set to `"true"` by default in the database,
# the converter would raise upon parsing.
#
# ```sql
# CREATE TABLE users (
#   id              INTEGER,
#   favorite_movies STRING  NOT NULL DEFAULT '{}'
# );
# ```
#
# ```
# require "onyx-sql/converters/sqlite3"
#
# class User
#   include Onyx::SQL::Model
#
#   schema do
#     type id : Int32, converter: SQLite3::Any(Int32)
#     type favorite_movies : Array(String), converter: SQLite3::Any(String)
#   end
# end
#
# Onyx::SQL::Converters::SQLite3::Any(Int32).new.to_db(42)       # => 42
# Onyx::SQL::Converters::SQLite3::Any(Int32).new.to_db([42, 43]) # => "{42, 43}"
# Onyx::SQL::Converters::SQLite3::Any(Int32).new.to_db(true)     # => "{1}"
# ```
module Onyx::SQL::Converters::SQLite3::Any(T)
  protected def self.check
    {% unless T <= DB::Any && !T.union? %}
      {% raise "Type #{T} is not supported by #{@type}. Consider using another converter" %}
    {% end %}
  end

  def self.to_db(value : T) : DB::Any
    check
    value
  end

  def self.to_db(values : Array(T)) : DB::Any
    check

    values.join(',') do |value|
      {% if T <= Number || T <= String %}
        value.to_s
      {% elsif T <= Bool %}
        value ? '1' : '0'
      {% elsif T <= Time %}
        value.in(::SQLite3::TIME_ZONE).to_s(::SQLite3::DATE_FORMAT)
      {% elsif T <= Nil %}
        ""
      {% elsif T <= Bytes %}
        {% raise "Must not use Bytes type with #{@type}. Consider using another specialized converter" %}
      {% end %}
    end.try { |v| "{#{v}}" }
  end

  def self.from_rs(rs : DB::ResultSet) : T?
    check
    rs.read(T | Nil)
  end

  def self.from_rs_array(rs : DB::ResultSet) : ::Array(T)?
    check

    rs.read(String | Nil).try do |string|
      sub = string[1..-2] rescue ""

      if sub.empty?
        return ::Array(T).new
      else
        sub.split(',').map do |s|
          {% if T <= Int32 %}
            s.to_i32
          {% elsif T <= Int64 %}
            s.to_i64
          {% elsif T <= Float32 %}
            s.to_f32
          {% elsif T <= Float64 %}
            s.to_f64
          {% elsif T <= Nil %}
            s == "" ? nil : raise "Unexpected non-empty SQLite3 array entry '#{s}'"
          {% elsif T <= String %}
            s
          {% elsif T <= Bool %}
            case s
            when '1' then true
            when '0' then false
            else
              raise "Unexpected non-bit SQLite3 array entry '#{s}'"
            end
          {% elsif T <= Bytes %}
            {% raise "Must not use Bytes type with #{@type}. Consider using another specialized converter" %}
          {% end %}
        end
      end
    end
  end
end
