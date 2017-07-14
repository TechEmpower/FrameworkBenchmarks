require "kemalyst-model/adapter/pg"

class World < Kemalyst::Model
  adapter pg
  table_name world
  primary id : Int32
  field randomNumber : Int32
end

