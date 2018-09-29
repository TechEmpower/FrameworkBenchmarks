require "granite/adapter/pg"

class Fortune < Granite::Base
  adapter pg

  table_name fortune
  primary id : Int32
  field message : String
end
