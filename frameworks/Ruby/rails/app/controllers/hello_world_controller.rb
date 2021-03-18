# frozen_string_literal: true

class HelloWorldController < ApplicationController
  QUERY_RANGE = (1..10_000).to_a

  def plaintext
    render plain: 'Hello, World!'
  end

  def json
    render json: { message: 'Hello, World!' }
  end

  def db
    render json: World.find(Random.rand(1..10_000))
  end

  def query
    results = QUERY_RANGE.sample(query_count).map do |id|
      World.find(id)
    end

    render json: results
  end

  def cached_query
    results = QUERY_RANGE.sample(query_count).map do |id|
      Rails.cache.fetch(id) do
        World.find(id).as_json
      end
    end

    render json: results
  end

  def fortune
    @fortunes = Fortune.all.to_a
    @fortunes << Fortune.new(id: 0, message: 'Additional fortune added at request time.')
    @fortunes.sort_by!(&:message)
  end

  def update
    worlds = query_count.times.map { Random.rand(1..10_000) }.map do |id|
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.find(id)
      random = Random.rand(1..10_000)
      random = Random.rand(1..10_000) until random != world.randomNumber
      world.update_columns(randomNumber: random)
      world
    end

    render json: worlds
  end

  private

  def query_count
    queries = params[:queries].to_i
    return 1 if queries < 1
    return 500 if queries > 500

    queries
  end
end
