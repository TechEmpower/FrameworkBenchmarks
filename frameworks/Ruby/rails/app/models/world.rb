class World < ApplicationRecord
  self.table_name = "World"

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')
end
