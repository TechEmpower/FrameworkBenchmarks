require 'active_record'
Bundler.require :default

set :logging, false
ActiveRecord::Base.logger = nil
set :activerecord_logger, nil
set :static, false
set :template_engine, :slim
Slim::Engine.set_default_options format: :html5, sort_attrs: false

# Specify the encoder - otherwise, sinatra/json inefficiently
# attempts to load one of several on each request
set :json_encoder => :to_json

# Don't prefix JSON results with { "world": {...} }
ActiveRecord::Base.include_root_in_json = false

db_config = { :database => 'hello_world', :username => 'benchmarkdbuser', :password => 'benchmarkdbpass', :pool => 256, :timeout => 5000 }
adapter = RUBY_PLATFORM == 'java' ? 'jdbcmysql' : 'mysql2'
set :database, db_config.merge(:adapter => adapter, :host => ENV['DB_HOST'])

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
end

class Fortune < ActiveRecord::Base
  self.table_name = "Fortune"
end

get '/json' do
  json :message => 'Hello, World!'
end

get '/plaintext' do
  content_type 'text/plain'
  'Hello, World!'
end

get '/db' do
  worlds = ActiveRecord::Base.connection_pool.with_connection do
    World.find(Random.rand(10000) + 1)
  end
  json(worlds)
end

get '/queries' do
  queries = (params[:queries] || 1).to_i
  queries = 1 if queries < 1
  queries = 500 if queries > 500

  worlds = ActiveRecord::Base.connection_pool.with_connection do
    (1..queries).map do
      World.find(Random.rand(10000) + 1)
    end
  end
  json(worlds)
end

get '/fortunes' do
  @fortunes = Fortune.all
  @fortunes << Fortune.new(:id => 0, :message => "Additional fortune added at request time.")
  @fortunes = @fortunes.sort_by { |x| x.message }

  slim :fortunes
end

get '/updates' do
  queries = (params[:queries] || 1).to_i
  queries = 1 if queries < 1
  queries = 500 if queries > 500

  worlds = ActiveRecord::Base.connection_pool.with_connection do
    worlds = (1..queries).map do
      world = World.find(Random.rand(10000) + 1)
      world.randomNumber = Random.rand(10000) + 1
      world
    end
    World.import worlds, :on_duplicate_key_update => [:randomNumber]
    worlds
  end
  json(worlds)
end
