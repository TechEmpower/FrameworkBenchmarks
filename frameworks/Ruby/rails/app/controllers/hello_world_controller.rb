class HelloWorldController < ApplicationController

  def plaintext
    render :plain => "Hello, World!"
  end

  def json
    render :json => {:message => "Hello, World!"}
  end

  def db
    render :json => World.find(Random.rand(10000) + 1)
  end

  def query
    queries = params[:queries].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    results = (1..queries).map do
      World.find(Random.rand(10000) + 1)
    end
    render :json => results
  end
  
  def fortune
    @fortunes = Fortune.all
    @fortunes << Fortune.new(:id => 0, :message => "Additional fortune added at request time.")
    @fortunes = @fortunes.sort_by { |x| x.message }
  end

  def update
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    worlds = (1..queries).map do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.find(Random.rand(10000) + 1)
      world.randomNumber = Random.rand(10000) + 1
      world
    end
    World.import worlds, :on_duplicate_key_update => [:randomNumber]
    render :json => worlds
  end
end