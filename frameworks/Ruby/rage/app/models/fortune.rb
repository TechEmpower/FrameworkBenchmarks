class Fortune < ApplicationRecord
  self.table_name = "Fortune"

  def as_json(*)
    attributes
  end
end
