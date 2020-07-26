# frozen_string_literal: true

class HelloWorldController < ApplicationController
  def plaintext
    render plain: 'Hello, World!'
  end

  def json
    render json: { message: 'Hello, World!' }
  end

  def db
    render json: World.find(Random.rand(1..10000))
  end

  def query
    queries = params[:queries].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    numbers = (1..10000).to_a.sample(queries)
    results = numbers.map do |id|
      World.find(id)
    end

    render json: results
  end

  def fortune
    @fortunes = Fortune.all.to_a
    @fortunes << Fortune.new(id: 0, message: 'Additional fortune added at request time.')
    @fortunes = @fortunes.sort_by(&:message)
  end

  def update
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    numbers = (1..10000).to_a.sample(queries).sort
    worlds = numbers.map do |id|
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.select(:id, :randomNumber).find(id)
      begin
        rn = Random.rand(1..10000)
      end while rn == world.randomNumber
      world.update_attribute(:randomNumber, rn)
      world
    end

    render json: worlds
  end
end
