class World < BaseModel
  skip_default_columns

  table :world do
    primary_key id : Int32
    column randomnumber : Int32
  end
end
