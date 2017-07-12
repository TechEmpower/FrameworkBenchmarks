require "kemalyst-model/adapter/pg"

class Fortune < Kemalyst::Model
  adapter pg
  table_name fortune
  primary id : Int32
  field message : String
end
