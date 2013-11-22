require "sinatra"
require "sinatra/json"
require "sinatra/activerecord"

set :logging, false
set :activerecord_logger, nil

if RUBY_PLATFORM == 'java'
  set :database, { :adapter => 'jdbc', :jndi => 'java:comp/env/jdbc/hello_world', :pool => 256 }
else
  set :database, { :adapter => 'mysql2', :database => 'hello_world', :username => 'benchmarkdbuser', :password => 'benchmarkdbpass', :host => 'localhost', :pool => 256, :timeout => 5000 }
end

class World < ActiveRecord::Base
  self.table_name = "World"
  attr_accessible :randomNumber
end

get '/json' do
  json :message => 'Hello World!'
end

get '/plaintext' do
  content_type 'text/plain'
  'Hello, World!'
end

get '/db' do
  queries = (params[:queries] || 1).to_i

  results = (1..queries).map do
    World.find(Random.rand(10000) + 1)
  end
  
  results.to_json
end