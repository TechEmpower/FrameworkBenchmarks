class World < ApplicationRecord
  self.table_name = "World"

  def as_json(*)
    attributes
  end

  alias_attribute(:randomNumber, :randomnumber)
end
