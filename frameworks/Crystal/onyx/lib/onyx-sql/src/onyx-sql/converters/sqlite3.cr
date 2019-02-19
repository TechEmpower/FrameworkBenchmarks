require "sqlite3"

# A collection of modules to convert to and from `SQLite3` database values.
# Depends on <https://github.com/crystal-lang/crystal-sqlite3>.
module Onyx::SQL::Converters::SQLite3
end

require "./sqlite3/any"
require "./sqlite3/enum_int"
require "./sqlite3/enum_text"
