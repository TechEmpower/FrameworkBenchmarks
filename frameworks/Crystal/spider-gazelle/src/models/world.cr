class World < Granite::Base
  connection pg
  table world

  column id : Int32, primary: true
  column randomnumber : Int32
end
