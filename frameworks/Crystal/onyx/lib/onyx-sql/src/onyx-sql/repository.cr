require "./repository/*"

module Onyx::SQL
  # A gateway between `Serializable` and DB. Its main features are logging,
  # expanding `Onyx::SQL::Query` instances and serializing from resulting `DB::ResultSet`.
  #
  # ```
  # db = DB.open(ENV["DATABASE_URL"])
  # repo = Onyx::SQL::Repository.new(db)
  #
  # repo.scalar("SELECT 1").as(Int32)
  # # [sql] SELECT 1
  # # 593μs
  #
  # repo.scalar("SELECT ?", 1).as(Int32)
  # # ditto
  #
  # repo.query("SELECT * FROM users")       # Returns raw `DB::ResultSet`
  # repo.query(User, "SELECT * FROM users") # Returns `Array(User)`
  # repo.query(User.all)                    # Returns `Array(User)` as well
  # # [sql] SELECT users.* FROM users
  # # 442μs
  # ```
  class Repository
    # A `DB::Database` instance for this repository.
    property db

    # A `Repository::Logger` instance for this repository.
    property logger

    # Initialize the repository.
    def initialize(@db : DB::Database, @logger : Logger = Logger::Standard.new)
    end

    protected def postgresql?
      {% if Object.all_subclasses.any? { |sc| sc.stringify == "PG::Driver" } %}
        return db.is_a?(PG::Driver)
      {% end %}

      return false
    end

    # Prepare query for initialization.
    #
    # If the `#db` driver is `PG::Driver`, replace all `?` with `$1`, `$2` etc. Otherwise return *sql_query* untouched.
    def prepare_query(sql_query : String)
      {% begin %}
        case db.driver
        {% if Object.all_subclasses.any? { |sc| sc.stringify == "PG::Driver" } %}
          when PG::Driver
            counter = 0
            sql_query.gsub("?") { '$' + (counter += 1).to_s }
        {% end %}
        else sql_query
        end
      {% end %}
    end

    # Return `#db` driver name, e.g. `"postgresql"` for `PG::Driver`.
    def driver
      {% begin %}
        case db.driver
        {% if Object.all_subclasses.any? { |sc| sc.stringify == "PG::Driver" } %}
          when PG::Driver then "postgresql"
        {% end %}
        {% if Object.all_subclasses.any? { |sc| sc.stringify == "SQLite3::Driver" } %}
          when SQLite3::Driver then "sqlite3"
        {% end %}
        else "sql"
        end
      {% end %}
    end
  end
end
