class HelloWorldController < ApplicationController

  def json
    render :json => {:message => "Hello World!"}
  end

  def db
    queries = (params[:queries] || 1).to_i

    results = (1..queries).map do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      World.find(Random.rand(10000) + 1)
    end
    render :json => results
  end
end
