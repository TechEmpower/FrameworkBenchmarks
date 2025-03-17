class World < ApplicationRecord
  self.table_name = "World"

  alias_attribute(:randomNumber, :randomnumber)
end
