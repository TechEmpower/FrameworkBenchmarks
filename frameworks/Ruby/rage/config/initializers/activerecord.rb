# frozen_string_literal: true

require "etc"

connection = {
  adapter: "postgresql",
  host: "tfb-database",
  username: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  reaping_frequency: 0,
  pool: (2 * Math.log(256 / Etc.nprocessors)).floor
}

puts "ActiveRecord connection options: #{connection.inspect}"

ActiveRecord::Base.establish_connection(connection)
