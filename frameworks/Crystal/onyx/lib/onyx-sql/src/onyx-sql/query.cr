require "./query/*"

module Onyx::SQL
  # Type-safe-where-possible SQL query builder.
  #
  # ## Cheatsheet
  #
  # For your convenience, here is the full list of `Query` methods:
  #
  # * Shortcuts:
  #   * `#one`
  #   * `#all`
  #   * `#first`
  #   * `#last`
  #   * `#and`
  #   * `#or`
  #   * `#and_not`
  #   * `#or_not`
  # * `#insert` -- type-safe
  # * `#update` -- must call `Set` afterwards ↴
  # * Set:
  #   * `#set` -- type-safe
  #   * `#set(clause)` -- expilicit clause
  #   * `#set(clause, params)` -- explicit clause with params
  # * Returning (currently not type-safe):
  #   * `#returning(values)`
  #   * `#returning(klass)` (link is broken, scroll a little)
  # * Select (currently not type-safe):
  #   * `#select(values)`
  #   * `#select(klass)` (link is broken, scroll a little)
  #   * `#select(klass, args)`
  # * Where:
  #   * `#where` -- type-safe
  #   * [`#where(clause)`](#where%28clause%3AString%2Cor%3ABool%3Dfalse%2Cnot%3ABool%3Dfalse%29-instance-method) -- expilicit clause
  #   * `#where(clause, params)` -- explicit clause with params
  #   * `#where_not(clause)`
  #   * `#where_not(clause, params)`
  #   * `#and_where`
  #   * `#and_where(clause)`
  #   * `#and_where(clause, params)`
  #   * `#and_where_not`
  #   * `#and_where_not(clause)`
  #   * `#and_where_not(clause, params)`
  #   * `#or_where`
  #   * `#or_where(clause)`
  #   * `#or_where(clause, params)`
  #   * `#or_where_not`
  #   * `#or_where_not(clause)`
  #   * `#or_where_not(clause, params)`
  # * Join:
  #   * [`#join`](#join%28%2A%2Con%3AString%3F%3Dnil%2Cas_as%3AString%3F%3Dnil%2Ctype%3AJoinType%3D%3Ainner%2C%2A%2Avalues%3A%2A%2AU%2C%26block%29%3AselfforallU-instance-method) -- type-safe
  #   * [`#join(reference, on, as, type)`](#join%28reference%3ASymbol%2Con%3AString%3F%3Dnil%2Cas_as%3AString%3F%3Dnil%2Ctype%3AJoinType%3D%3Ainner%29-instance-method) -- not type-safe by reference
  #   * `#join(table, on, as, type)` -- explicit
  # * `#group_by(string_values)`
  # * Having (doesn't have type-safe methods):
  #   * [`#having(clause)`](file:///home/faust/Projects/onyxframework/sql/docs/Onyx/SQL/Query.html#having%28clause%3AString%2Cor%3ABool%3Dfalse%2Cnot%3ABool%3Dfalse%29-instance-method) -- expilicit clause
  #   * `#having(clause, params)` -- explicit clause with params
  #   * `#having_not(clause)`
  #   * `#having_not(clause, params)`
  #   * `#and_having(clause)`
  #   * `#and_having(clause, params)`
  #   * `#and_having_not(clause)`
  #   * `#and_having_not(clause, params)`
  #   * `#or_having(clause)`
  #   * `#or_having(clause, params)`
  #   * `#or_having_not(clause)`
  #   * `#or_having_not(clause, params)`
  # * `#limit(number)`
  # * `#offset(number)`
  # * `#order_by(value, order)` -- currently not type-safe
  #
  # ## Basics
  #
  # `Query` is a generic class and it can be only of a `Model` type (not bare `Serializable`s).
  # The model must have `Model::Options` annotation with both `:table` and `:primary_key` options set.
  #
  # `Query` implements most of the SQL syntax. It does **not** make queries to a DB itself,
  # it only **builds** the SQL string with SQL-ready params.
  # Its main purposes are to make the application less error-prone with type-safety and reduce
  # the usage of raw SQL within the code-base. However, it's **not** a complete replacement
  # for SQL. You have to know SQL and may be would have to still use it in some cases.
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   schema users do
  #     pkey id : Int32
  #   end
  # end
  #
  # query = Query(User).new  # OK
  # query = Query(Int32).new # Totally not OK
  # ```
  #
  # To build a query, call `#build` on it:
  #
  # ```
  # query.select("*")
  # query.build # => {"SELECT users.* FROM users", <EmptyParams>}
  # ```
  #
  # Once the query is built, pass it either to a raw `DB` instance:
  #
  # ```
  # sql, params = query.build
  # result_set = db.query(sql, params)
  #
  # # Or you can *splat* the two-element Tuple resulting from Query#build
  # result_set = db.query(*query.build)
  #
  # users = User.from_rs(result_set)
  # pp typeof(users) # => Array(User)
  # ```
  #
  # Or give it to a `Repository` instance so it builds it and extracts its generic type implicitly:
  #
  # ```
  # users = repo.query(query)
  #
  # # query has User as its generic type, and the repo knows it
  # pp typeof(users) # => Array(User)
  # ```
  #
  # As `#build` returns a pair of raw SQL string and params, you can combine its usage as you want:
  #
  # ```
  # db.scalar(*query.build)
  # db.exec(query.build[0]) # Will omit params
  # repo.scalar(query)
  # repo.query(User, query.build[0], query.build[1])
  # repo.exec(*query)
  # ```
  #
  # ## Type safety
  #
  # Some of the `Query` methods are type-safe. For example, `#where`:
  #
  # ```
  # query = Query(User).new.where(name: "John")
  # ```
  #
  # In this example, the query would look for an instance variable with getter called `#name`
  # in the `User` class. And if it does not find it, it raises in compilation time with meaningful
  # error message:
  #
  # ```text
  # Class `User` has neither field nor reference with key `:name` eligible for `Query(User)#where` call
  # ```
  #
  # And if you pass the wrong type to the `where` call, it would raise as well:
  #
  # ```
  # query.where(name: 42)
  # ```
  #
  # ```text
  # Invalid compile-time type `Int32` for argument `:name` in `Query(User)#where` call. Expected: `(String | Nil)`
  # ```
  #
  # `#insert`, `#join` and `#set` are also type-safe. Other methods could be type-safe in theory,
  # but there is an [issue](https://forum.crystal-lang.org/t/symbols/391) blocking it.
  # *All* `Query` methods are expected to be type-safe in the future.
  #
  # ## Model shortucts
  #
  # `Model` has both class and instance shortcuts. Class shortcuts are available for almost
  # every `Query` method, for example:
  #
  # ```
  # User.where(id: 42) == Query(User).new.where(id: 42)
  # ```
  #
  # See `Model::ClassQueryShortcuts`. Instance shortucts are `Model#insert`, `Model#update`
  # and `Model#delete`.
  class Query(T)
    # The query type.
    enum Type
      Insert
      Select
      Update
      Delete
    end

    # The query type. It's automatically updated on appropriate methods call
    # (e.g. it changes to `:insert` on `#insert` call)
    getter type : Type = Type::Select

    # The table alias for this query. For example, `"SELECT myalias.*"` insted of `"SELECT users.*"`.
    @alias : String? = nil

    # :nodoc:
    def initialize(@alias : String? = nil)
    end

    # Compare queries.
    def ==(other : self)
      return false unless type == other.type
      return false unless get_limit == other.get_limit
      return false unless get_offset == other.get_offset
      return false unless get_group_by == other.get_group_by
      return false unless get_order_by == other.get_order_by
      return false unless get_returning == other.get_returning
      return false unless self.get_select == other.get_select
      return false unless get_having == other.get_having
      return false unless get_insert == other.get_insert
      return false unless get_join == other.get_join
      return false unless get_set == other.get_set
      return false unless get_where == other.get_where

      return true
    end

    # Alias of `#limit(nil)`.
    def all
      limit(nil)
    end

    # Alias of `#limit(1)`.
    def one
      limit(1)
    end

    # Alias of `#order_by(primary_key, :asc).one`. The primary key is determined by the
    # `Model::Options` `:primary_key` option.
    def first
      order_by(primary_key, :asc).one
    end

    # Alias of `#order_by(primary_key, :desc).one`. The primary key is determined by the
    # `Model::Options` `:primary_key` option.
    def last
      order_by(primary_key, :desc).one
    end

    # Return the SQL representation of this query.
    # Pass `true` to replace `"?"` query arguments with `"$n"`, which would work for PostgreSQL.
    def to_s(index_params = false)
      io = IO::Memory.new
      to_s(io, params: nil, index_params: index_params)
      io.to_s
    end

    # Put the SQL representation of this query into the *io*.
    # Pass `true` for *index_params* to replace `"?"` query arguments with `"$n"`,
    # which would work for PostgreSQL.
    def to_s(io, index_params = false)
      to_s(io, params: nil, index_params: index_params)
    end

    # Build this query, returning its SQL representation and `Enumerable` of DB-ready params.
    # Pass `true` to replace `"?"` query arguments with `"$n"`, which would work for PostgreSQL.
    def build(index_params = false) : Tuple(String, Enumerable(DB::Any))
      sql = IO::Memory.new
      params = Array(DB::Any).new

      to_s(sql, params, index_params)

      return sql.to_s, params
    end

    # :nodoc:
    # Which clause - `WHERE` or `HAVING` was called the latest?
    enum LatestWherishClause
      Where
      Having
    end

    @latest_wherish_clause : LatestWherishClause = :where

    {% for joinder in %w(and or) %}
      {% for not in [true, false] %}
        # A shorthand for calling `{{joinder.id}}_where{{"_not".id if not}}` or `{{joinder.id}}_having{{"_not".id if not}}` depending on the latest call (`#where` by default).
        #
        # BUG: It will raise in runtime if called after `having`.
        def {{joinder.id}}{{"_not".id if not}}(**args : **U) forall U
          if @latest_wherish_clause == LatestWherishClause::Having
            raise "BUG: Cannot call 'Onyx::SQL::Query(#{T})#having with named arguments'"
          else
            {{joinder.id}}_where{{"_not".id if not}}(**args)
          end
        end

        # ditto
        def {{joinder.id}}{{"_not".id if not}}(clause : String, *params)
          if @latest_wherish_clause == LatestWherishClause::Having
            {{joinder.id}}_having{{"_not".id if not}}(clause, *params)
          else
            {{joinder.id}}_where{{"_not".id if not}}(clause, *params)
          end
        end

        # ditto
        def {{joinder.id}}{{"_not".id if not}}(clause : String)
          if @latest_wherish_clause == LatestWherishClause::Having
            {{joinder.id}}_having{{"_not".id if not}}(clause)
          else
            {{joinder.id}}_where{{"_not".id if not}}(clause)
          end
        end
      {% end %}
    {% end %}

    protected def to_s(io, params = nil, index_params = false)
      index = index_params ? ParamIndex.new : nil

      case type
      when Type::Insert
        append_insert(io, params, index)
        append_returning(io, params, index)
      when Type::Select
        append_select(io, params, index)
        append_join(io, params, index)
        append_where(io, params, index)
        append_group_by(io, params, index)
        append_having(io, params, index)
        append_order_by(io, params, index)
        append_limit(io, params, index)
        append_offset(io, params, index)
      when Type::Update
        append_update(io, params, index)
        append_set(io, params, index)
        append_where(io, params, index)
        append_returning(io, params, index)
      when Type::Delete
        append_delete(io, params, index)
        append_where(io, params, index)
        append_returning(io, params, index)
      else
        raise "BUG: Uhandled type #{type}"
      end
    end

    private class ParamIndex
      property value = 0
    end

    protected def primary_key
      {% begin %}
        {%
          options = T.annotation(Model::Options)
          raise "Onyx::SQL::Model::Options annotation must be defined for #{T}" unless options

          pk = options[:primary_key]
          raise "Onyx::SQL::Model::Options annotation is missing :primary_key option for #{T}" unless pk

          pk_ivar = T.instance_vars.find { |riv| "@#{riv.name}".id == pk.id }
          raise "Cannot find primary key field #{pk} for #{T}" unless pk_ivar
        %}

        {{pk_ivar.name.symbolize}}
      {% end %}
    end
  end
end
