class BenchmarkController < Amber::Controller::Base
  LAYOUT     = "main.ecr"
  HTML_UTF8  = "text/html; charset=UTF-8"
  JSON       = "application/json"
  TEXT_PLAIN = "text/plain"
  ID_MAXIMUM = 10_000

  before_action do
    all do
      response.headers["Server"] = "Amber"
      response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")
    end
  end

  def plaintext
    response.content_type = TEXT_PLAIN
    "Hello, World!"
  end

  def json
    response.content_type = JSON
    {message: "Hello, World!"}.to_json
  end

  def db
    response.content_type = JSON
    results = {} of Symbol => Int32
    if world = World.find rand(1..ID_MAXIMUM)
      results = {id: world.id, randomNumber: world.randomNumber}
    end
    results.to_json
  end

  def queries
    response.content_type = JSON
    queries = params["queries"]
    queries = queries.to_i? || 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..ID_MAXIMUM)
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def updates
    response.content_type = JSON
    queries = params["queries"]
    queries = queries.to_i? || 1
    queries = queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..ID_MAXIMUM)
        world.randomNumber = rand(1..ID_MAXIMUM)
        world.save
        {id: world.id, randomNumber: world.randomNumber}
      end
    end

    results.to_json
  end

  def fortunes
    response.content_type = HTML_UTF8
    fortune = Fortune.new
    fortune.id = 0
    fortune.message = "Additional fortune added at request time."

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message || "" }

    render("fortune/index.ecr")
  end
end
