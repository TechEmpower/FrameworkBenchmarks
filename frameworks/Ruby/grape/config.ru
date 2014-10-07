require 'erb'
require 'active_record'

Bundler.require :default

db_config = YAML.load(ERB.new(File.read("config/database.yml")).result)[ENV['RACK_ENV']]
ActiveRecord::Base.establish_connection(db_config)

class World < ActiveRecord::Base
  self.table_name = "World"
end

module Acme
  class HelloWorld < Grape::API
    get '/json' do
      {message:"Hello, World!"}
    end
  end

  class PlainText < Grape::API
    content_type :plain, "text/plain; charset=utf-8"
    format :plain
    get '/plaintext' do
      "Hello, World!"
    end
  end

  class DatabaseQueries < Grape::API
    get '/db' do
      ActiveRecord::Base.connection_pool.with_connection do
        World.find(Random.rand(10000) + 1)
      end
    end

    get '/query' do
      queries = params[:queries].to_i
      queries = 1 if queries < 1
      queries = 500 if queries > 500

      ActiveRecord::Base.connection_pool.with_connection do
        results = (1..queries).map do
          World.find(Random.rand(10000) + 1)
        end
      end
    end

    get '/updates' do
      queries = params[:queries].to_i
      queries = 1 if queries < 1
      queries = 500 if queries > 500

      ActiveRecord::Base.connection_pool.with_connection do
        worlds = (1..queries).map do
          world = World.find(Random.rand(10000) + 1)
          world.randomNumber = Random.rand(10000) + 1
          world
        end
        World.import worlds, :on_duplicate_key_update => [:randomNumber]
        worlds
      end
    end
  end

  class API < Grape::API
    content_type :json, "application/json; charset=utf-8"
    format :json
    
    mount ::Acme::HelloWorld
    mount ::Acme::PlainText
    mount ::Acme::DatabaseQueries
  end
end

run Acme::API


