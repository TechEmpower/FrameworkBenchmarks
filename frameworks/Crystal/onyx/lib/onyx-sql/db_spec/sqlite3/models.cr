require "../../spec/models"
require "../../src/onyx-sql/converters/sqlite3"
require "../../src/onyx-sql/converters/sqlite3/uuid_blob"
require "../../src/onyx-sql/converters/sqlite3/json"

@[Onyx::SQL::Model::Options(table: "users", primary_key: @id)]
class User
  @[Field(key: "rowid", converter: SQLite3::Any(Int32))]
  property! id : Int32

  @[Field(converter: SQLite3::UUIDBlob, default: true)]
  @uuid : UUID?

  @[Field(converter: SQLite3::EnumInt(User::Role), default: true)]
  @role : Role?

  @[Field(converter: SQLite3::EnumText(User::Permission), default: true)]
  @permissions : Array(Permission)?

  @[Field(converter: SQLite3::Any(Int32), default: true)]
  @favorite_numbers : Array(Int32)?

  @[Field(converter: SQLite3::JSON(User::Meta), default: true)]
  @meta : Meta?

  @[Onyx::SQL::Reference(key: "referrer_id")]
  @referrer : self?

  @[Onyx::SQL::Reference(foreign_key: "referrer_id")]
  @referrals : Array(self)?

  @[Onyx::SQL::Reference(foreign_key: "author_id")]
  @authored_posts : Array(Post)?

  @[Onyx::SQL::Reference(foreign_key: "editor_id")]
  @edited_posts : Array(Post)?
end

class Tag
  macro finished
    @[Field(key: "rowid", converter: SQLite3::Any(Int32))]
    @id : Int32?
  end
end

class Post
  macro finished
    @[Field(key: "rowid", converter: SQLite3::Any(Int32))]
    @id : Int32?

    @[Onyx::SQL::Reference(key: "author_id")]
    @author : User?

    @[Onyx::SQL::Reference(key: "editor_id")]
    @editor : User?
  end
end
