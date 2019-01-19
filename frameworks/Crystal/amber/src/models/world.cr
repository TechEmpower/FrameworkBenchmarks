require "granite/adapter/pg"

class World < Granite::Base
  adapter pg

  table_name world
  primary id : Int32
  field randomnumber : Int32
end
