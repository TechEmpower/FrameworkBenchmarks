class BenchmarkController < Amber::Controller::Base
  LAYOUT = "main.ecr"

  def plaintext
    "Hello, World!"
  end

  def json
    results = {message: "Hello, World!"}
    results.to_json
  end

  def db
    results = {} of Symbol => Int32
    if world = World.find rand(1..10_000)
      results = {id: world.id, randomNumber: world.randomNumber}
    end
    results.to_json
  end

  def queries
    queries = params["queries"].as(String)
    queries = queries.to_i? || 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..10_000)
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def updates
    queries = params["queries"].as(String)
    queries = queries.to_i? || 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      world = World.find rand(1..10_000)
      if world
        world.randomNumber = rand(1..10_000)
        world.save
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def fortunes
    fortune = Fortune.new
    fortune.id = 0_i64
    fortune.message = "Additional fortune added at request time."

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message.not_nil! }

    render("fortune/index.ecr")
  end
end
