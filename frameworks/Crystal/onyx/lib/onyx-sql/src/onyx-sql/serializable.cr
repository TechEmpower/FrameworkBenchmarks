require "./converters"

# Include this module to make an object serializable **from** `DB::ResultSet`.
# This is particulary useful for reusable business objects. In an `Onyx::SQL::Serializable` object,
# **all** its instance variables must be nilable:
#
# ```crystal
# struct PopularProducts
#   include Onyx::SQL::Serializable
#
#   @id : Int32?
#   @popularity : Float64?
# end
#
# rs = db.query("SELECT id, popularity ...")
# popular_products = PopularProducts.from_rs(rs) # => Array(PopularProducts)
# ```
#
# Remember that a row's column names must match the object's instance variables.
# You still can use `Field` and `Reference` annotations, though:
#
# ```crystal
# struct PopularProducts
#   include Onyx::SQL::Serializable
#
#   # .from_rs would now expect a column named "the_id" instead of "id"
#   @[Onyx::SQL::Field(key: "the_id")]
#   @id : Int32?
# end
# ```
#
# Read more about `Reference` serialization below.
#
# You can also use `Repository#query` method as soon as it calls `.from_rs` with result set
# columns matching the desired keys. Keep that in mind when using `Query`, because it could
# return columns with invalid or unknown (to this serializable object) names, resulting in
# a `DB::MappingException` runtime error.
#
# `Model` includes `Serializable` and it also has `Model.schema` DSL macro, so if you want
# to map a model, using `Model` module over `Serializable` is preferrable.
#
# ## Serializing references
#
# Before reading this section, make yourself familiar with `Reference` annotation.
#
# Let's consider this classic example:
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
#     type body : String
#     type author : User, key: "author_id"
#   end
# end
# ```
#
# ### Direct non-enumerable references
#
# `Post#author` is a direct non-enumerable reference and it can be preloaded from a result set,
# as the row is linear and sufficient to store all the author's data:
#
# ```text
# | post_id | post_content | author_id | author_username |
# | ------- + ------------ + --------- + --------------- |
# | 17      | "Hello"      | 42        | "John"          |
# ```
#
# However, a `Post` can have multiple direct references and they all can have overlapping
# column names, so to avoid it, there is a concept of *select markers*. A marker wraps a reference's
# column in the result set, allowing to distinguish between references:
#
# ```text
# | id | content | _author | id | username | _author |
# | -- + ------- + ------- + -- + -------- + ------- +
# | 17 | "Hello" |         | 42 | "John"   |         |
# ```
#
# In this example, columns named `"_author"` are markers and they're essentialy empty strings.
# It results in ability to properly preload references, in this case:
#
# ```
# post = Post.from_rs(rs).first
# pp post # => <Post @id=17 @content="Hello" @author=<User @id=42 @username="John">>
# ```
#
# ### Foreign non-enumerable references
#
# In one-to-one relations, a model can have a foreign non-enumerable reference, for example:
#
# ```
# class User
#   schema users do
#     pkey id : Int32
#     type settings : Settings, foreign_key: "user_id"
#   end
# end
#
# class Settings
#   schema settings do
#     pkey id : Int32
#     type foo : String
#     type user : User, key: "user_id"
#   end
# end
# ```
#
# In this case, you can preload `User#settings` along with a `User` instance, because it
# fits a single row:
#
# ```
# user = User
#   .where(id: 42)
#   .select(:id, :username)
#   .join(settings: true) do |q|
#     q.select(:foo)
#   end
# end
# ```
#
# The row would look like this:
#
# ```text
# | id | username | _settings | foo   | _settings |
# | -- + -------- + --------- + ----- + --------- +
# | 42 | "John"   |           | "bar" |           |
# ```
#
# And it would be perfectly parseable by `.from_rs`.
#
# ### Enumerable references
#
# Now let's consider `User#authored_posts`, which is a foreign enumerable reference.
# You can not effectively put many posts in a single row, that's why it's impossible
# to preload enumerable references at all. If you want to get all posts authored by a user,
# you should query the `"posts"` table, not the `"users"`, and receive a collection of `Post` rows.
#
# This restriction applies both to direct and foreign *enumerable* references.
module Onyx::SQL::Serializable
  include Converters

  # It's a class because it's value needs to be changed within recursive calls.
  private class ColumnIndexer
    property value = 0
  end

  # Initialize an array of `self` from a database result set.
  # Raises `DB::MappingException` if there is an unknown column, so
  # make sure the result set's rows contain only well-known columns.
  def self.from_rs(rs : DB::ResultSet) : Array(self)
  end

  macro included
    def self.from_rs(rs : DB::ResultSet) : Array(self)
      instances = [] of self

      rs.each do
        instance = allocate
        instance.initialize(rs)
        GC.add_finalizer(instance) if instance.responds_to?(:finalize)

        instances << instance
      end

      instances
    ensure
      rs.close
    end

    macro inherited
      def self.from_rs(rs : DB::ResultSet)
        super
      end
    end
  end

  # :nodoc:
  protected def initialize(
    rs : DB::ResultSet,
    stop_column : String? = nil,
    column_indexer = ColumnIndexer.new
  )
    {% for ivar in @type.instance_vars %}
      {%
        a = 42 # Dummy assignment, otherwise the compiler errors

        unless ivar.type.nilable?
          raise "#{@type}@#{ivar.name} must be nilable, as it's an Onyx::SQL::Serializable variable"
        end

        unless ivar.type.union_types.size == 2
          raise "Only T | Nil unions can be an Onyx::SQL::Serializable's variables (got #{ivar.type} type for #{@type}@#{ivar.name})"
        end
      %}
    {% end %}

    vars_set = Hash(String, Bool).new

    while column_indexer.value < rs.column_count
      column_name = rs.column_name(column_indexer.value)

      {% begin %}
        case column_name
          when stop_column
            rs.read
            column_indexer.value += 1
            break
          {% for ivar in @type.instance_vars.reject { |iv| iv.annotation(Onyx::SQL::Reference) } %}
            {%
              key = ((ann = ivar.annotation(Onyx::SQL::Field)) && ann[:key]) || ivar.name
              converter = (ann = ivar.annotation(Onyx::SQL::Field)) && ann[:converter]

              type = ivar.type.union_types.find { |t| t != Nil }
              enumerable = type <= Enumerable
            %}

            when {{key.id.stringify}}
              {% if converter %}
                {% if enumerable %}
                  raw = {{converter}}.from_rs_array(rs)

                  if raw
                    @{{ivar.name}} = {{type}}.new
                    raw.each { |r| @{{ivar.name}}.not_nil! << r.as({{type.type_vars.first}}) }
                  end
                {% else %}
                  @{{ivar.name}} = {{converter}}.from_rs(rs)
                {% end %}
              {% elsif type <= DB::Any %}
                @{{ivar.name}} = rs.read({{ivar.type}})
              {% else %}
                {% raise "Cannot implicitly read field #{@type}@#{ivar.name} : #{ivar.type} from a DB::ResultSet at key #{key.id.stringify}, because its type is not <= DB::Any. Consider applying a converter to #{@type}@#{ivar.name} to make it work" %}
              {% end %}

              vars_set[{{ivar.name.stringify}}] = true
              column_indexer.value += 1
          {% end %}

          {% for ivar in @type.instance_vars.select { |iv| (ann = iv.annotation(Onyx::SQL::Reference)) && ann[:key] } %}
            {%
              key = ivar.annotation(Onyx::SQL::Reference)[:key].id

              type = ivar.type.union_types.find { |t| t != Nil }
              rtype = type
              renumerable = false

              if type <= Enumerable
                renumerable = true
                rtype = type.type_vars.first
              end

              roptions = rtype.annotation(Onyx::SQL::Model::Options)
              raise "Onyx::SQL::Model::Options annotation must be defined for #{rtype}" unless roptions

              rpk = roptions[:primary_key]
              raise "#{rtype} must have Onyx::SQL::Model::Options annotation with :primary_key option " unless rpk

              rpk_ivar = rtype.instance_vars.find { |riv| "@#{riv.name}".id == rpk.id }
              raise "Cannot find primary key field #{rpk} in #{rtype}" unless rpk_ivar

              rpk_converter = (a = rpk_ivar.annotation(Onyx::SQL::Field)) && a[:converter]

              rpk_ivar_type = rpk_ivar.type.union_types.find { |t| t != Nil }
            %}

            when {{key.stringify}}
              {% if renumerable %}
                pkeys = {% if rpk_converter %}
                  {{rpk_converter}}.from_rs_array(rs)
                {% else %}
                  {% raise "Cannot implicitly read a direct enumerable reference #{@type}@#{ivar.name} from a DB::ResultSet at key #{key.id.stringify}. Consider applying a converter with `#from_rs_array` method to #{rtype}@#{rpk_ivar.name} to make it work" %}
                {% end %}

                if pkeys
                  @{{ivar.name}} = {{type}}.new

                  pkeys.each do |pkey|
                    @{{ivar.name}}.not_nil! << {{type.type_vars.first}}.new({{rpk_ivar.name}}: pkey.as({{rpk_ivar_type}}))
                  end
                end
              {% else %}
                pkey = {% if rpk_converter %}
                  {{rpk_converter}}.from_rs(rs)
                {% elsif rpk_ivar_type <= DB::Any %}
                  rs.read({{rpk_ivar.type}})
                {% else %}
                  {% raise "Cannot implicitly read a direct reference #{@type}@#{ivar.name} from a DB::ResultSet at key #{key.id.stringify}, because its primary key #{rtype}@#{rpk_ivar.name} type is #{rpk_ivar_type}, which is not <= DB::Any. Consider applying a converter to #{rtype}@#{rpk_ivar.name} to make it work" %}
                {% end %}

                if pkey
                  @{{ivar.name}} = {{type}}.new({{rpk_ivar.name}}: pkey.as({{rpk_ivar_type}}))
                end
              {% end %}

              vars_set[{{ivar.name.stringify}}] = true
              column_indexer.value += 1

            # Do not preload joined enumerable references (e.g. multiple tags in one row)
            {% unless type <= Enumerable %}
              when "_{{ivar.name}}"
                rs.read
                column_indexer.value += 1

                @{{ivar.name}} = {{type}}.allocate
                @{{ivar.name}}.not_nil!.initialize(rs, "_{{ivar.name}}", column_indexer)

                vars_set[{{ivar.name.stringify}}] = true
            {% end %}
          {% end %}
        else
          raise DB::MappingException.new("Cannot deserialize column '#{column_name}' for {{@type}} from a result set at index #{column_indexer.value}")
        end
      {% end %}
    end
  end
end
