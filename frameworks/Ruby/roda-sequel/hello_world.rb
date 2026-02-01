# frozen_string_literal: true
require 'bundler/setup'
Bundler.require(:default) # Load core modules

require_relative 'db'
require 'time'

# Our Rack application to be executed by rackup
class HelloWorld < Roda
  MAX_PK = 10_000
  QUERY_RANGE = (1..MAX_PK).freeze
  ALL_IDS = QUERY_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500

  CONTENT_TYPE = 'Content-Type'
  JSON_TYPE = 'application/json'
  HTML_TYPE = 'text/html; charset=utf-8'
  PLAINTEXT_TYPE = 'text/plain'
  DATE_HEADER = 'Date'
  SERVER_HEADER = 'Server'
  SERVER_STRING = 'roda'

  plugin :hooks
  plugin :render, escape: true, layout_opts: { cache_key: "default_layout" }
  plugin :default_headers, SERVER_HEADER => SERVER_STRING

  def bounded_queries
    queries = request.params["queries"].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK) + 1
  end

  route do |r|
    response[DATE_HEADER] = Time.now.httpdate if defined?(Puma)

    # Test type 1: JSON serialization
    r.is "json" do
      response[CONTENT_TYPE] = JSON_TYPE
      { message: "Hello, World!" }.to_json
    end

    # Test type 2: Single database query
    r.is "db" do
      response[CONTENT_TYPE] = JSON_TYPE
      World.with_pk(rand1).values.to_json
    end

    # Test type 3: Multiple database queries
    r.is "queries" do
      response[CONTENT_TYPE] = JSON_TYPE
      ids = ALL_IDS.sample(bounded_queries)
      worlds =
        DB.synchronize do
          ids.map do |id|
            World.with_pk(id).values
          end
        end
      worlds.to_json
    end

    # Test type 4: Fortunes
    r.is "fortunes" do
      response[CONTENT_TYPE] = HTML_TYPE
      @fortunes = Fortune.all

      fortune = Fortune.new
      fortune.id = 0
      fortune.message = "Additional fortune added at request time."
      @fortunes << fortune

      @fortunes.sort_by!(&:message)
      view :fortunes
    end

    # Test type 5: Database updates
    r.is "updates" do
      response[CONTENT_TYPE] = JSON_TYPE
      worlds = []
      ids = ALL_IDS.sample(bounded_queries)
      DB.synchronize do
        worlds =
          ids.map do |id|
            world = World.with_pk(id)
            new_value = rand1
            new_value = rand1 while new_value == world.randomnumber
            world.randomnumber = new_value
            world
          end
        World.batch_update(worlds)
      end
      worlds.map!(&:values).to_json
    end

    # Test type 6: Plaintext
    r.is "plaintext" do
      response[CONTENT_TYPE] = PLAINTEXT_TYPE
      "Hello, World!"
    end
  end
end
