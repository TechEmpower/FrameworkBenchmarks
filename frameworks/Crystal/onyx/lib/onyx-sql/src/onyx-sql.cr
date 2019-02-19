require "db"
require "./onyx-sql/*"

# An SQL ORM for Crystal.
module Onyx::SQL
  # Use this annotation to mark an Object's instance variable as an SQL field.
  # It's not mandatory, though, as including `Serializable` and `Model` has meaningful defaults.
  # In particular, the serialization process would rely on a variable name
  # when mapping a database column by default. You can change this behaviour with `:key` option.
  # For example:
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #   @id : Int32?
  # end
  #
  # User.db_column(:id)    # => "id"
  # User.db_values(id: 42) # => 42
  # ```
  #
  # By default, the serialization code would look for column named `"id"`.
  # But you can change it:
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   @[Onyx::SQL::Field(key: "the_id")]
  #   @id : Int32?
  # end
  #
  # User.db_column(:id) # => "the_id"
  # ```
  #
  # Now the serialization will map from `"the_id"` column to the `@id` instance variable.
  #
  # Furthermore, there is a `:converter` option which would define a converter to use
  # for the serialization. For example, you have an integer enum column with value `0`
  # stored in an SQLite database. In this case, the `Converters::SQLite3::EnumInt` would be helpful:
  #
  # ```
  # class User
  #   enum Role
  #     Writer
  #     Moderator
  #   end
  #
  #   @[Onyx::SQL::Field(converter: Onyx::SQL::Converters::SQLite3::EnumInt(Role))]
  #   @role : Role
  # end
  #
  # User.db_values(role: User::Role::Writer) # => 1
  # ```
  #
  # From now on, the serialization would expect an `INT` column and try to parse
  # the `User::Role` enum out from it.
  #
  # You can use both `:key` and `:converter` options simultaneously on a variable.
  #
  # ## Usage in schema
  #
  # `Model.schema` DSL macro automatically passes `Model.type` and `Model.pkey` options to a `Field` annotation,
  # unless it's a `Model` reference. See `Reference` docs.
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   schema users do
  #     pkey id : Int32, converter: PG::Any(Int32)
  #     type username : String, key: "the_username"
  #   end
  # end
  #
  # # Expands to
  #
  # @[Onyx::SQL::Model::Options(table: "users", primary_key: @id)]
  # class User
  #   include Onyx::SQL::Model
  #
  #   @[Onyx::SQL::Field(converter: Onyx::SQL::Converters::PG::Any(Int32))]
  #   property! id : Int32
  #
  #   @[Onyx::SQL::Field(key: "the_username")]
  #   property! username : String
  # end
  # ```
  annotation Field
  end

  # Use this annotation to mark an Object's variable as an SQL Reference.
  # Both `Model` and bare `Serializable` object can have `Reference` instance variables.
  #
  # You have to decide what type of reference to use. There are two options -- *direct references*
  # and *foreign references*.
  #
  # ## Direct references
  #
  # Direct reference should be understood as *this record stores an another record reference
  # in this instance variable* and it is determined by the `:key` option. The referenced object
  # **must** have `Model::Options` annotation with `:table` and `:primary_key` options.
  # Additionaly, a matching accessor must be defined for the primary key (e.g. `User#id`
  # in the example below)):
  #
  # ```
  # @[Onyx::SQL::Model::Options(table: "users", primary_key: @id)]
  # class User
  #   include Onyx::SQL::Model
  #   property! id : Int32
  # end
  #
  # class Post
  #   include Onyx::SQL::Model
  #
  #   @[Onyx::SQL::Reference(key: "author_id")]
  #   property! author : User
  # end
  # ```
  #
  # In this example, a `Post` might have a `User` instance stored in the `@author` variable,
  # which is a *direct reference*. When making an SQL query, this instance is implicitly cast to
  # a database type -- an `"author_id"` column with `Int32` type:
  #
  # ```
  # Post.db_column(:author)           # => "author_id"
  # Post.db_values(author: post.user) # => 42 (the user's ID)
  # ```
  #
  # If a reference is direct (i.e. has the `:key` option), a referenced instance is initialized
  # as soon as a database result set reads that key. For example, if the result set has "author_id"
  # key with value `42`, the `Post` instance will be initialized as `<Post @author=<User @id=42>>`.
  # And if you want to preload a reference field (or sub-reference), you should use `JOIN`.
  # See `Query#join` for more details.
  #
  # You can make both enumerable and non-enumerable variables references.
  # It is impossible to preload enumerable references, though, because the result set is read
  # row-by-row.
  #
  # ## Foreign references
  #
  # Let's extend the previous example:
  #
  # ```
  # @[Onyx::SQL::Model::Options(table: "users", primary_key: @id)]
  # class User
  #   include Onyx::SQL::Model
  #
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
  #   property! id : Int32
  #
  #   @[Onyx::SQL::Reference(key: "author_id")]
  #   property! author : User
  # end
  # ```
  #
  # As you may notice, the `User` class now got the `authored_posts` reference and the `Post` class
  # now has the `Model::Options` annotation. A user has a list of all the posts authored by them,
  # which is essentialy a *foreign reference*. Basically, the ORM requires that both classes
  # have the link defined -- a direct reference in the first and a foreign in the second. But
  # don't you worry, it will raise in compilation time and tell you about that.
  #
  # Foreign references can be joined as well, but it also implies the inability to preload enumerable
  # references. However, it works with single foreign references like in this example:
  #
  # ```
  # class User
  #   @[Onyx::SQL::Reference(foreign_key: "user_id")]
  #   property! settings : Settings
  # end
  #
  # class Settings
  #   property! foo : String
  #
  #   @[Onyx::SQL::Reference(key: "user_id")]
  #   property! user : User
  # end
  #
  # user = repo.query(User.join(:settings)(&.select(:foo)).where(id: 42))
  # pp user # => <User @id=42 @settings=<Settings @foo="bar">>
  # ```
  #
  # NOTE: You must not use both `:key` and `:foreign_key` options on a single instance variable.
  #
  # ## Usage in schema
  #
  # `Model.schema` DSL macro effectively reduces and beautifies the code,
  # as the `Reference` annotation is automatically applied if a `Model.type` type is `Model` itself
  # and has either `:key` or `:foreign_key` option:
  #
  # ```
  # class User
  #   include Onyx::SQL::Model
  #
  #   schema users do
  #     pkey id : Int32, converter: PG::Any(Int32)
  #     type authored_posts : Array(Post), foreign_key: "author_id" # This
  #   end
  # end
  #
  # class Post
  #   include Onyx::SQL::Model
  #
  #   schema posts do
  #     pkey id : Int32, converter: PG::Any(Int32)
  #     type author : User, key: "author_id" # And this
  #   end
  # end
  # ```
  annotation Reference
  end
end
