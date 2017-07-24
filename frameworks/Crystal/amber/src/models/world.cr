require "granite_orm/adapter/pg"

class World < Granite::ORM
  adapter pg

  table_name world
  field randomNumber : Int32
end
