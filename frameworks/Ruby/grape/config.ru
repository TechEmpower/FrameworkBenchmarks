require 'erb'
require 'active_record'
require 'yaml'
require_relative 'config/auto_tune'

MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500

Bundler.require :default

db_config = YAML.load(ERB.new(File.read('config/database.yml')).result)[ENV['RACK_ENV']]
ActiveRecord::Base.establish_connection(db_config)

class World < ActiveRecord::Base
  self.table_name = 'World'
end

module Acme
  class HelloWorld < Grape::API
    get '/json' do
      {message:'Hello, World!'}
    end
  end

  class PlainText < Grape::API
    content_type :plain, 'text/plain'
    format :plain

    get '/plaintext' do
      'Hello, World!'
    end
  end

  class DatabaseQueries < Grape::API
    helpers do
      def bounded_queries
        queries = params[:queries].to_i
        queries.clamp(QUERIES_MIN, QUERIES_MAX)
      end

      # Return a random number between 1 and MAX_PK
      def rand1
        rand(MAX_PK).succ
      end
    end

    get '/db' do
      ActiveRecord::Base.connection_pool.with_connection do
        World.find(rand1).attributes
      end
    end

    get '/query' do
      ActiveRecord::Base.connection_pool.with_connection do
        Array.new(bounded_queries) do
          World.find(rand1)
        end
      end
    end

    get '/updates' do
      worlds =
        ActiveRecord::Base.connection_pool.with_connection do
          Array.new(bounded_queries) do
            world = World.find(rand1)
            new_value = rand1
            new_value = rand1 while new_value == world.randomNumber
            world.update_columns(randomNumber: new_value)
            world
          end
        end
    end
  end

  class API < Grape::API
    before do
      header 'Date', Time.now.httpdate
      header 'Server', 'WebServer'
    end
    content_type :json, 'application/json'
    format :json

    mount ::Acme::HelloWorld
    mount ::Acme::PlainText
    mount ::Acme::DatabaseQueries
  end
end

run Acme::API
