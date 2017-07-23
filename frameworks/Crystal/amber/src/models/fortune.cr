require "granite_orm/adapter/pg"

class Fortune < Granite::ORM 
  adapter pg

  field message : String
  timestamps
end
