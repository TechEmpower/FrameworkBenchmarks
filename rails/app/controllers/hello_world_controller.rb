class HelloWorldController < ApplicationController

  def json
    render :json => {:message => "Hello, World!"}
  end

  def db
    queries = (params[:queries] || 1).to_i

    if queries > 1
      results = (1..queries).map do
        # get a random row from the database, which we know has 10000
        # rows with ids 1 - 10000
        World.find(Random.rand(10000) + 1)
      end
    else
      results = World.find(Random.rand(10000) + 1)
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

    results = (1..queries).map do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.find(Random.rand(10000) + 1)
      world.randomNumber = Random.rand(10000) + 1
      world.save

      world
    end
    render :json => results
  end
end
