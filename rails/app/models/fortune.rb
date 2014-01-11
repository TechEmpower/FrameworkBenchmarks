class Fortune < ActiveRecord::Base
  self.table_name = "Fortune"
  attr_accessible :id, :message
end