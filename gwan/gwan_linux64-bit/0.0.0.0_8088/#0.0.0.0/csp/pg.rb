#!/usr/bin/ruby
# Test type 2: Single database query

require 'json'
require_relative 'db-connect-pg'
require_relative 'gwan-helper'

id = rand(1..10000)
content = ''
res = DB.query "SELECT id, randomNumber FROM World WHERE id = #{id} LIMIT 1"

res.each do |row|
  content = row.to_json
  break
end

respond content, 'application/json'
