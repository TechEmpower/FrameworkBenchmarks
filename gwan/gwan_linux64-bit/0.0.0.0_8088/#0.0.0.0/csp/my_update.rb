#!/usr/bin/ruby
# Test type 5: Database updates

require 'json'
require_relative 'db-connect-my'
require_relative 'gwan-helper'

params = request

queries = params[:queries].to_i
queries = 1 if queries < 1
queries = 500 if queries > 500

content = []
queries.times do
  id = rand(1..10000)
  res = DB.query "SELECT id, randomNumber FROM World WHERE id = #{id} LIMIT 1"
  new = rand(1..10000)
  DB.query "UPDATE World SET randomNumber = #{new} WHERE id = #{id}"
  res.each do |row|
    row[:randomNumber] = new
    content << row
    break
  end
end
content = content.to_json

respond content, 'application/json'

