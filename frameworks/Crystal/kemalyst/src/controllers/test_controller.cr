require "../models/*"

class TestController < Kemalyst::Controller
  action headers do |context|
    context.response.headers["Server"] = "Kemalyst"
    context.response.headers["Date"] = Time.now.to_s
    call_next(context)
  end

  action plaintext do
    text "Hello, World!"
  end

  action json do
    json({message: "Hello, World!"}.to_json)
  end

  action db do
    if world = World.find rand(1..10_000)
      json({id: world.id, randomNumber: world.randomNumber}.to_json)
    end
  end

  action queries do
    queries = params["queries"].as(String)
    queries = queries.to_i? || 1
    queries.clamp(1..500)

    results = (1..queries).map do
      if world = World.find rand(1..10_000)
        {id: world.id, randomNumber: world.randomNumber}.to_json
      end
    end

    json results.to_json
  end

  action updates do
    queries = params["queries"].as(String)
    queries = queries.to_i? || 1
    queries.clamp(1..500)

    updated = (1..queries).map do
      world = World.find rand(1..10_000)
      if world
        world.randomNumber = rand(1..10_000)
        world.save
        {id: world.id, randomNumber: world.randomNumber}.to_json
      end
    end

    json updated.to_json
  end

  action fortunes do
    fortunes = Fortune.all
    fortunes << Fortune.new({id: 0, message: "Additional fortune added at request time."}.to_h)
    fortunes.sort_by! { |fortune| fortune.not_nil!.message.not_nil! }

    html render("fortune/index.slang", "main.slang")
  end
end
