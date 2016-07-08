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
      test = 124
      header 'Date', Time.now.to_s
      header 'Server', ENV['NEWRELIC_DISPATCHER']
      {message:"Hello, World!"}
    end
  end

  class PlainText < Grape::API
    content_type :plain, "text/plain"
    format :plain
    get '/plaintext' do
      test = 124
      header 'Date', Time.now.to_s
      header 'Server', ENV['NEWRELIC_DISPATCHER']
      "Hello, World!"
    end
  end

  class DatabaseQueries < Grape::API
    get '/db' do
      test = 124
      header 'Date', Time.now.to_s
      header 'Server', ENV['NEWRELIC_DISPATCHER']
      ActiveRecord::Base.connection_pool.with_connection do
        World.find(Random.rand(10000) + 1)
      end
    end

    get '/query' do
      test = 124
      header 'Date', Time.now.to_s
      header 'Server', ENV['NEWRELIC_DISPATCHER']
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
      header 'Date', Time.now.to_s
      header 'Server', ENV['NEWRELIC_DISPATCHER']
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
    content_type :json, "application/json"
    format :json

    mount ::Acme::HelloWorld
    mount ::Acme::PlainText
    mount ::Acme::DatabaseQueries
  end
end

run Acme::API
