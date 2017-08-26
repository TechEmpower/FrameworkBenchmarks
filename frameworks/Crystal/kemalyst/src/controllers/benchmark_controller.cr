require "../models/*"

class BenchmarkController < Kemalyst::Controller
  def plaintext
    text "Hello, World!"
  end

  def json
    results = {message: "Hello, World!"}
    json results.to_json
  end

  def db
    results = {} of Symbol => Int32
    if world = World.find rand(1..10_000)
      results = {id: world.id, randomNumber: world.randomNumber}
    end
    json results.to_json
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

    json results.to_json
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

    json results.to_json
  end

  def fortunes
    fortune = Fortune.new
    fortune.id = 0
    fortune.message = "Additional fortune added at request time."

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message.not_nil! }

    html render("fortune/index.slang", "main.slang")
  end
end

