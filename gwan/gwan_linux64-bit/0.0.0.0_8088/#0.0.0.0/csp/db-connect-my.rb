#!/usr/bin/ruby
require 'mysql2'

Mysql2::Client.default_query_options.merge!(symbolize_keys: true)
DB = Mysql2::Client.new(host: "localhost", username: "benchmarkdbuser", password: 'benchmarkdbpass')
DB.select_db('hello_world')
