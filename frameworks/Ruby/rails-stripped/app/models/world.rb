class World < ActiveRecord::Base
  self.table_name = "World"
  attr_accessible :randomNumber
end