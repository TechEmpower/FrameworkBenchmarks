#!/usr/bin/ruby
require 'pg'
DB = PG::Connection.new('localhost',5432,nil,nil,'hello_world','benchmarkdbuser','benchmarkdbpass')
