class World < ActiveRecord::Base
  self.table_name = name

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')
end
