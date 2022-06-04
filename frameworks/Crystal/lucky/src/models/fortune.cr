class Fortune < BaseModel
  skip_default_columns

  table :fortune do
    primary_key id : Int32
    column message : String
  end
end
