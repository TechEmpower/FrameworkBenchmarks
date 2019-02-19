require "./spec_helper"
require "./dummy_converters/*"

require "uuid"
require "json"

alias Model = Onyx::SQL::Model
alias Field = Onyx::SQL::Field

@[Model::Options(table: "users", primary_key: @uuid)]
class User
  include Model

  enum Role
    Writer
    Moderator
    Admin
  end

  enum Permission
    CreatePosts
    EditPosts
  end

  struct Meta
    include JSON::Serializable
    property foo : String?

    def initialize(@foo = nil)
    end
  end

  @[Field(converter: DummyConverters::UUID)]
  @uuid : UUID?
  property! uuid

  @[Field(key: "activity_status", default: true)]
  property! active : Bool

  @[Field(converter: DummyConverters::Enum(Role), default: true)]
  property! role : Role

  @[Field(converter: DummyConverters::Enum(Permission), default: true)]
  property! permissions : Array(Permission)

  @[Field(converter: DummyConverters::Int32Array, default: true)]
  property! favorite_numbers : Array(Int32)

  property! name : String

  @[Field(default: true)]
  property! balance : Float32

  @[Field(converter: DummyConverters::JSON(Meta), default: true)]
  property! meta : Meta

  @[Field(default: true)]
  property! created_at : Time

  property! updated_at : Time

  @[Onyx::SQL::Reference(key: "referrer_uuid")]
  property! referrer : self

  @[Onyx::SQL::Reference(foreign_key: "referrer_uuid")]
  property! referrals : Array(self)

  @[Onyx::SQL::Reference(foreign_key: "author_uuid")]
  property! authored_posts : Array(Post)

  @[Onyx::SQL::Reference(foreign_key: "editor_uuid")]
  property! edited_posts : Array(Post)

  # def initialize(@name : String, @uuid : UUID? = nil, @active : Bool? = nil)
  # end
end

class Tag
  include Model

  schema tags do
    pkey id : Int32
    type content : String
    type posts : Array(Post), foreign_key: "tag_ids"
  end
end

class Post
  include Model

  schema posts do
    pkey id : Int32, converter: DummyConverters::Int32Array

    type content : String
    type created_at : Time, default: true
    type updated_at : Time

    type author : User, key: "author_uuid"
    type editor : User, key: "editor_uuid"
    type tags : Array(Tag), key: "tag_ids"
  end
end
