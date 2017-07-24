require "granite_orm/adapter/pg"

class Fortune < Granite::ORM
  adapter pg

  table_name fortune
  field message : String
end
