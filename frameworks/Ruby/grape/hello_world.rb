# frozen_string_literal: true

require 'bundler/setup'
Bundler.require :default

require_relative 'db'

module Acme
  MAX_PK = 10_000
  ID_RANGE = (1..MAX_PK).freeze
  ALL_IDS = ID_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500

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
    logger nil
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
      ActiveRecord::Base.with_connection do
        World.find(rand1).attributes
      end
    end

    get '/query' do
      ActiveRecord::Base.with_connection do
        ALL_IDS.sample(bounded_queries).map do |id|
          World.find(id).attributes
        end
      end
    end

    get '/updates' do
      worlds = nil
      ids = ALL_IDS.sample(bounded_queries)
      ActiveRecord::Base.with_connection do
        worlds = ids.map do |id|
          world = World.find(id)
          new_value = rand1
          new_value = rand1 until new_value != world.randomNumber
          { id: id, randomnumber: new_value }
        end
      end
      worlds.sort_by!{_1[:id]}
      ActiveRecord::Base.with_connection do
        World.upsert_all(worlds)
      end
      worlds
    end
  end

  class API < Grape::API
    before do
      header 'Date', Time.now.httpdate if defined?(Puma)
      header 'Server', 'grape'
    end
    logger nil
    content_type :json, 'application/json'
    format :json

    mount ::Acme::HelloWorld
    mount ::Acme::PlainText
    mount ::Acme::DatabaseQueries
  end
end
