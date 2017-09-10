require "granite_orm/adapter/pg"

class World < Granite::ORM::Base
  adapter pg

  table_name world
  primary id : Int32
  field randomNumber : Int32
end
