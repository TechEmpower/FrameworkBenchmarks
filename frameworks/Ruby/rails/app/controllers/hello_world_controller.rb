# frozen_string_literal: true

class HelloWorldController < ActionController::API
  include DateHeader

  QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1            # min number of records that can be retrieved
  MAX_QUERIES = 500          # max number of records that can be retrieved

  def db
    render json: World.find(random_id).attributes
  end

  def query
    results = ALL_IDS.sample(query_count).map do |id|
      World.find(id).attributes
    end

    render json: results
  end

  def cached_query
    keys = ALL_IDS.sample(query_count).map { "world_#{_1}" }
    items = Rails.cache.fetch_multi(*keys) do |id|
      raise "Could not find World with id: #{id} in cache"
    end

    render json: items.values
  end

  def update
    worlds = ALL_IDS.sample(query_count).map do |id|
      world = World.find(id)
      new_value = random_id
      new_value = random_id until new_value != world.randomNumber
      { id: id, randomNumber: new_value }
    end
    World.upsert_all(worlds.sort_by!{_1[:id]})

    render json: worlds
  end

  private

  def query_count
    queries = params[:queries].to_i
    queries.clamp(MIN_QUERIES, MAX_QUERIES)
  end

  def random_id
    Random.rand(QUERY_RANGE)
  end
end
