require "granite_orm/adapter/pg"

class World < Granite::ORM
  adapter pg
  table_name world
  primary id : Int32
  field randomNumber : Int32
end

