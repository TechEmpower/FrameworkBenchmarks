require "granite_orm/adapter/pg"

class World < Granite::ORM 
  adapter pg

  field randomNumber : Int32
  timestamps
end
