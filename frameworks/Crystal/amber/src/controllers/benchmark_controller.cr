class BenchmarkController < Amber::Controller::Base
  LAYOUT = "main.ecr"

  def initialize(@context)
    super(@context)
    response.headers["Server"] = "Amber"
    response.headers["Date"] = Time.now.to_s
  end

  def plaintext
    response.content_type = "text/plain"
    "Hello, World!"
  end

  def json
    response.content_type = "application/json"
    results = {message: "Hello, World!"}
    results.to_json
  end

  def db
    response.content_type = "application/json"
    results = {} of Symbol => Int32
    if world = World.find rand(1..10_000)
      results = {id: world.id, randomNumber: world.randomNumber}
    end
    results.to_json
  end

  def queries
    response.content_type = "application/json"
    queries = params["queries"]?
    queries = queries ? queries.to_i? || 1 : 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..10_000)
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def updates
    response.content_type = "application/json"
    queries = params["queries"]?
    queries = queries ? queries.to_i? || 1 : 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..10_000)
        world.randomNumber = rand(1..10_000)
        world.save
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def fortunes
    response.content_type = "text/html"    
    fortune = Fortune.new
    fortune.id = 0_i64
    fortune.message = "Additional fortune added at request time."

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message || "" }

    render("fortune/index.ecr")
  end
end
