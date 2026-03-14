# frozen_string_literal: true

require "etc"

pool_size = (2 * Math.log(256 / Etc.nprocessors)).floor
puts "ActiveRecord pool size: #{pool_size}"

ENV["DATABASE_URL"]="postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?pool=#{pool_size}&reaping_frequency=0"

ActiveRecord::Base.logger = nil
