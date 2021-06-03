# frozen_string_literal: true

class HelloWorldController < ApplicationController
  QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1            # min number of records that can be retrieved
  MAX_QUERIES = 500          # max number of records that can be retrieved

  def plaintext
    render plain: 'Hello, World!'
  end

  def json
    render json: { message: 'Hello, World!' }
  end

  def db
    render json: World.find(random_id)
  end

  def query
    results = ALL_IDS.sample(query_count).map do |id|
      World.find(id)
    end

    render json: results
  end

  def cached_query
    items = Rails.cache.fetch_multi(*ALL_IDS.sample(query_count)) do |id|
      World.find(id).as_json
    end

    render json: items.values
  end

  def fortune
    @fortunes = Fortune.all.to_a
    @fortunes << Fortune.new(id: 0, message: 'Additional fortune added at request time.')
    @fortunes.sort_by!(&:message)
  end

  def update
    worlds = query_count.times.map { random_id }.map do |id|
      world = World.find(id)
      new_value = random_id
      new_value = random_id until new_value != world.randomNumber
      world.update_columns(randomNumber: new_value)
      world
    end

    render json: worlds
  end

  private

  def query_count
    queries = params[:queries].to_i
    return MIN_QUERIES if queries < MIN_QUERIES
    return MAX_QUERIES if queries > MAX_QUERIES

    queries
  end

  def random_id
    Random.rand(QUERY_RANGE)
  end
end
