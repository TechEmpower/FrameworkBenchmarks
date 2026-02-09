# frozen_string_literal: true

require 'erb'
require 'active_record'
require 'yaml'

db_config = YAML.load(ERB.new(File.read('config/database.yml')).result)[ENV['RACK_ENV']]
ActiveRecord::Base.establish_connection(db_config)
ActiveRecord::Base.logger = nil

class World < ActiveRecord::Base
  self.table_name = 'World'

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')
end

