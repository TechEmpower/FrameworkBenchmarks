require "granite_orm/adapter/pg"

class Fortune < Granite::ORM::Base
  adapter pg

  table_name fortune
  primary id : Int32
  field message : String
end
