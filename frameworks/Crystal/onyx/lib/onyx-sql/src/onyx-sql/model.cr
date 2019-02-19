require "./model/*"
require "./serializable"
require "./converters"

# Model (also *record*) is a business unit.
# Models usually have fields and relations with other models.
# The `Model` module allows to represent an SQL model as a plain Crystal object.
#
# ```sql
# CREATE TABLE users (
#   id        SERIAL  PRIMARY KEY,
#   username  TEXT    NOT NULL
# );
#
# CREATE TABLE posts (
#   id        SERIAL  PRIMARY KEY,
#   content   TEXT    NOT NULL,
#   author_id INT     NOT NULL  REFERENCES users (id),
# );
# ```
#
# ```
# class User
#   include Onyx::SQL::Model
#
#   schema users do
#     pkey id : Int32
#     type username : String
#     type authored_posts : Array(Post), foreign_key: "author_id"
#   end
# end
#
# class Post
#   include Onyx::SQL::Model
#
#   schema posts do
#     pkey id : Int32
#     type content : String
#     type author : User, key: "author_id"
#   end
# end
# ```
#
# In this example, `User` and `Post` are models. `User` has primary key `id`, field `username` and
# foreign enumerable reference `authored_posts`. `Post` also has primary key `id`,
# field `content` and direct reference `author`. It's pretty simple and straightforward mapping.
# Read more about references in `Serializable` docs.
#
# ## Serialization
#
# `Model` module includes `Serializable`, which enables deserializing models from a `DB::ResultSet`,
# effectively allowing this:
#
# ```
# db = DB.open(ENV["DATABASE_URL"])
# users = User.from_rs(db.query("SELECT * FROM users"))
# ```
#
# But it's more convenient to use `Repository` to interact with the database:
#
# ```
# repo = Onyx::SQL::Repository.new(db)
# users = repo.query(User, "SELECT * FROM users")
# ```
#
# That's not much less code, but the repo, for example, handles query arguments
# (`?` -> `$1` for PostrgreSQL queries) and also logs the requests.
# The real power of repository is handling `Query` arguments:
#
# ```
# user = repo.query(User.where(id: 42)).first
# ```
#
# ## Schema
#
# Onyx::SQL is based on Crystal annotations to keep composition and simplify the underlying code.
# But since annotations are quite low-level, they are masked under the convenient `.schema` DSL.
# It's a good idea to understand what the `.schema` macro generates, but it's not mandatory
# for most of developers.
module Onyx::SQL::Model
  include Converters

  # It doesn't make sense, but it's seen in the API docs
  extend Onyx::SQL::Model::ClassQueryShortcuts

  macro included
    include Onyx::SQL::Serializable
    include Onyx::SQL::Model::Mappable
    extend Onyx::SQL::Model::ClassQueryShortcuts
  end

  # Compare `self` against *other* model of the same type by their primary keys.
  # Returns `false` if the `self` primary key is `nil`.
  def ==(other : self)
    {% begin %}
      {%
        options = @type.annotation(Onyx::SQL::Model::Options)
        raise "Onyx::SQL::Model::Options annotation must be defined for #{@type}" unless options

        pk = options[:primary_key]
        raise "#{@type} must have Onyx::SQL::Model::Options annotation with :primary_key option" unless pk

        pk_rivar = @type.instance_vars.find { |riv| "@#{riv.name}".id == pk.id }
        raise "Cannot find primary key field #{pk} in #{@type}" unless pk_rivar
      %}

      unless primary_key.nil?
        primary_key == other.{{pk_rivar.name}}
      end
    {% end %}
  end

  # Initialize an instance of `self`. It accepts an arbitrary amount of arguments,
  # but they must match the variable names, raising in compile-time instead:
  #
  # ```
  # User.new(id: 42, username: "John") # => <User @id=42 @username="John">
  # User.new(foo: "bar")               # Compilation-time error
  # ```
  def initialize(**values : **T) : self forall T
    {% for ivar in @type.instance_vars %}
      {%
        a = 42 # BUG: Dummy assignment, otherwise the compiler crashes

        unless ivar.type.nilable?
          raise "#{@type}@#{ivar.name} must be nilable, as it's an Onyx::SQL::Model variable"
        end

        unless ivar.type.union_types.size == 2
          raise "Only T | Nil unions can be an Onyx::SQL::Model's variables (got #{ivar.type} type for #{@type}@#{ivar.name})"
        end
      %}
    {% end %}

    values.each do |key, value|
      {% begin %}
        case key
        {% for key, value in T %}
          {% found = false %}

          {% for ivar in @type.instance_vars %}
            {% if ivar.name == key %}
              {% raise "Invalid type #{value} for #{@type}@#{ivar.name} (expected #{ivar.type})" unless value <= ivar.type %}

              when {{ivar.name.symbolize}}
                @{{ivar.name}} = value.as({{value}})

              {% found = true %}
            {% end %}
          {% end %}

          {% raise "Cannot find instance variable by key #{key} in #{@type}" unless found %}
        {% end %}
        else
          raise "BUG: Runtime key mismatch"
        end
      {% end %}
    end

    self
  end

  # This annotation specifies options for a `Model`. It has two mandatory options itself:
  #
  # * `:table` -- the table name in the DB, e.g. "users"
  # * `:primary_key` -- the primary key **variable**, for example:
  #
  # ```
  # @[Onyx::SQL::Options(table: "users", primary_key: @id)]
  # class User
  #   include Onyx::SQL::Model
  #   @id : Int32?
  # end
  # ```
  #
  # The `Model.schema` macro defines the `Options` annotation for you:
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   schema users do   # "users" is going to be the :table option
  #     pkey id : Int32 # @id is the :primary_key
  #   end
  # end
  # ```
  #
  # TODO: Handle different `:primary_key` variants:
  #
  # ```
  # @[Options(primary_key: {@a, @b})] # Composite
  #
  # @[Options(primary_key: {x: @a, y: @b})] # With different getters (and composite)
  # class User
  #   def x
  #     @a
  #   end
  # end
  # ```
  annotation Options
  end

  protected def_hash primary_key

  protected def primary_key
    {% begin %}
      {%
        options = @type.annotation(Onyx::SQL::Model::Options)
        raise "Onyx::SQL::Model::Options annotation must be defined for #{@type}" unless options

        pk = options[:primary_key]
        raise "#{@type} must have Onyx::SQL::Model::Options annotation with :primary_key option" unless pk

        pk_rivar = @type.instance_vars.find { |riv| "@#{riv.name}".id == pk.id }
        raise "Cannot find primary key field #{pk} in #{@type}" unless pk_rivar
      %}

      @{{pk_rivar}}
    {% end %}
  end
end
