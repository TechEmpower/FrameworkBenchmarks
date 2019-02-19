module Onyx::SQL::Model
  # `.schema` is a convenient DSL to avoid dealing with cumbersome (but extremely powerful)
  # annotations directly. Consider this code:
  #
  # ```
  # @[Onyx::SQL::Model::Options(table: "users", primary_key: @id)]
  # class User
  #   include Onyx::SQL::Model
  #
  #   @[Onyx::SQL::Field(converter: Onyx::SQL::Converters::PG::Any(Int32))]
  #   property! id : Int32
  #
  #   @[Onyx::SQL::Reference(foreign_key: "author_id")]
  #   property! authored_posts : Array(Post)
  # end
  #
  # @[Onyx::SQL::Model::Options(table: "posts", primary_key: @id)]
  # class Post
  #   include Onyx::SQL::Model
  #
  #   @[Onyx::SQL::Field(converter: Onyx::SQL::Converters::PG::Any(Int32))]
  #   property! id : Int32
  #
  #   @[Onyx::SQL::Reference(key: "author_id")]
  #   property! author : User
  # end
  # ```
  #
  # With the DSL, it could be simplifed to:
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
  # This macro has a single mandatory argument *table*, which is, obviously, the model's table name.
  # The schema currently **requires** a `.pkey` variable.
  #
  # TODO: Make the primary key optional.
  macro schema(table, &block)
    {{yield.id}}
    define_options({{table}})
  end

  # Declare a model field or reference, **must** be called within `.schema` block.
  # Expands to `property!`, which make the variable nilable, but it would
  # raise `NilAssertionError` if trying to access it's being `nil`. For example:
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   schema users do
  #     pkey id : Int32
  #     type name : String
  #   end
  # end
  #
  # user = repo.query(User.first.select(:id)).first # Note that we select the @id only
  #
  # pp user   # => <User @id=42 @name=nil>
  # user.id   # => 42
  # user.name # => NilAssertionError
  # ```
  #
  # If you are not sure if a variable is preloaded, use `?` methods:
  #
  # ```
  # if user.name?  # Will gently check if it's not `nil`
  #   pp user.name # "John"
  # end
  # ```
  macro type(declaration, **options)
    macro finished
      {% unless options.empty? %}
        \{%
          type = {{declaration.type}}

          if type.union?
            if type.union_types.size != 2
              raise "Only T | Nil unions can be an Onyx::SQL::Model's variables (got #{type} type for #{@type}@#{declaration.var})"
            end

            type = type.union_types.find { |t| t != Nil }
          end

          if type <= Enumerable
            if type.type_vars.size != 1
              raise "Cannot use #{type} as an Onyx::SQL instance variable for #{@type}"
            end

            type = type.type_vars.first
          end
        %}

        \{% if type < Onyx::SQL::Model %}
          \{{"@[Onyx::SQL::Reference(key: #{{{options[:key]}}}, foreign_key: #{{{options[:foreign_key]}}})]".id}}
        \{% else %}
          \{{"@[Onyx::SQL::Field(key: #{{{options[:key]}}}, default: #{{{options[:default]}}}, converter: #{{{options[:converter]}}})]".id}}
        \{% end %}
      {% end %}

      \{{"property! {{declaration}}".id}}
    end
  end

  # Declare a model primary key, **must** be called within `.schema` block. It is equal to `.type`,
  # but also defines the `:primary_key` option for the `Options` annotation.
  # It's currently mandatory to have a primary key in a model, which may change in the future.
  macro pkey(declaration, **options)
    private ONYX_SQL_MODEL_SCHEMA_PK = {{"@#{declaration.var}".id}}
    type({{declaration}}, {{**options}})
  end

  private macro define_options(table)
    {% raise "Primary key is not defined in #{@type} schema. Use `pkey` macro for this" unless ONYX_SQL_MODEL_SCHEMA_PK %}

    @[Onyx::SQL::Model::Options(table: {{table}}, primary_key: {{ONYX_SQL_MODEL_SCHEMA_PK}})]
    class ::{{@type}}
    end
  end
end
