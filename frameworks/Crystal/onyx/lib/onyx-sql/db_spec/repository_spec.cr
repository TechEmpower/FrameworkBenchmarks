require "./spec_helper"

enum Database
  Postgresql
  Sqlite3
end

def repo(database : Database)
  Onyx::SQL::Repository.new(DB.open(ENV["#{database.to_s.upcase}_URL"]), Onyx::SQL::Repository::Logger::Dummy.new)
end
