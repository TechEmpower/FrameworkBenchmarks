class HelloWorldController < ApplicationController
  def json
    render :json => {:message => "Hello World!"}
  end

  def db
    queries = params[:queries] || 1

    results = []
    (1..queries.to_i).each do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      results << World.find(Random.rand(10000) + 1)
    end
    render :json => results
  end
end