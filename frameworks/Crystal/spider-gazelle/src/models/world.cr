require "pg-orm"

class World < PgORM::Base
  attribute id : Int32, primary_key: true
  attribute randomnumber : Int32
end
