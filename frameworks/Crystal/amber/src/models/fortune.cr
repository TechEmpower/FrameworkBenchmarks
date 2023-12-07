require "granite/adapter/pg"

class Fortune < Granite::Base
  connection pg
  table fortune
  
  column id : Int32, primary: true
  column message : String
end
