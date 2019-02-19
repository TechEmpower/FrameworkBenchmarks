require "../../spec/models"
require "../../src/onyx-sql/converters/pg"
require "../../src/onyx-sql/converters/pg/uuid"
require "../../src/onyx-sql/converters/pg/json"

class User
  @[Field(converter: PG::UUID)]
  @uuid : UUID?

  @[Field(converter: PG::Enum(User::Role), default: true)]
  @role : Role?

  @[Field(converter: PG::Enum(User::Permission), default: true)]
  @permissions : Array(Permission)?

  @[Field(converter: PG::Any(Int32), default: true)]
  @favorite_numbers : Array(Int32)?

  @[Field(converter: PG::JSON(User::Meta), default: true)]
  @meta : Meta?
end

class Post
  macro finished
    @[Field(converter: PG::Any(Int32))]
    property! id : Int32
  end
end

class Tag
  macro finished
    @[Field(converter: PG::Any(Int32))]
    property! id : Int32
  end
end
