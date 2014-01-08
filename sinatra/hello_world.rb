require "sinatra"
require "sinatra/json"
require "sinatra/activerecord"

set :logging, false
set :activerecord_logger, nil
set :static, false

# Specify the encoder - otherwise, sinatra/json inefficiently
# attempts to load one of several on each request
set :json_encoder => :to_json

if RUBY_PLATFORM == 'java'
  set :database, { :adapter => 'jdbcmysql', :database => 'hello_world', :username => 'benchmarkdbuser', :password => 'benchmarkdbpass', :host => 'localhost', :pool => 256, :timeout => 5000 }
else
  set :database, { :adapter => 'mysql2', :database => 'hello_world', :username => 'benchmarkdbuser', :password => 'benchmarkdbpass', :host => 'localhost', :pool => 256, :timeout => 5000 }
end

# The sinatra-activerecord gem registers before and after filters that
# call expensive synchronized ActiveRecord methods on every request to
# verify every connection in the pool, even for routes that don't use
# the database. Clear those filters and handle connection management
# ourselves, which is what applications seeking high throughput with
# ActiveRecord need to do anyway.
settings.filters[:before].clear
settings.filters[:after].clear

class World < ActiveRecord::Base
  self.table_name = "World"
  attr_accessible :randomNumber
end

get '/json' do
  json :message => 'Hello, World!'
end

get '/plaintext' do
  content_type 'text/plain'
  'Hello, World!'
end

get '/db' do
  queries = (params[:queries] || 1).to_i

  ActiveRecord::Base.connection_pool.with_connection do
    results = (1..queries).map do
      World.find(Random.rand(10000) + 1)
    end

    json results
  end
end
