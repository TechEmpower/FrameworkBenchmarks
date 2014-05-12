#!/usr/bin/ruby
# Test type 3: Multiple database queries

require 'json'
require_relative 'db-connect-pg'
require_relative 'gwan-helper'

params = request

queries = params[:queries].to_i
queries = 1 if queries < 1
queries = 500 if queries > 500

content = []
queries.times do
  id = rand(1..10000)
  res = DB.query "SELECT id, randomNumber FROM World WHERE id = #{id} LIMIT 1"
  res.each do |row|
    content << row
    break
  end
end
content = content.to_json

respond content, 'application/json'
