require "pg"
require "onyx-sql"

class Fortune
  include Onyx::SQL::Model

  schema fortune do
    pkey id : Int32
    type message : String
  end
end
