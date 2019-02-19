require "onyx-sql"
require "./logger"
require "./db"

class Onyx
  # Top-level `Onyx::SQL::Repository instance`.
  # Has `Onyx.db` as a *db* and and `Onyx.logger` as a *logger*.
  class_property repo : Onyx::SQL::Repository = Onyx::SQL::Repository.new(
    Onyx.db,
    Onyx::SQL::Repository::Logger::Standard.new(Onyx.logger, ::Logger::Severity::DEBUG)
  )

  # Call `Onyx::SQL::Repository#query` on the top-level `.repo` instance.
  def self.query(*args, **nargs)
    repo.query(*args, **nargs)
  end

  # Call `Onyx::SQL::Repository#exec` on the top-level `.repo` instance.
  def self.exec(*args, **nargs)
    repo.exec(*args, **nargs)
  end

  # Call `Onyx::SQL::Repository#scalar` on the top-level `.repo` instance.
  def self.scalar(*args, **nargs)
    repo.scalar(*args, **nargs)
  end
end
