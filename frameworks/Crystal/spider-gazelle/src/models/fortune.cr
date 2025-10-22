require "pg-orm"

class Fortune < PgORM::Base
  attribute id : Int32, primary_key: true
  attribute message : String
end
