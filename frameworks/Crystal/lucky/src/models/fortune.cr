class Fortune < BaseModel
  table :fortune do
    column message : String
  end
end
