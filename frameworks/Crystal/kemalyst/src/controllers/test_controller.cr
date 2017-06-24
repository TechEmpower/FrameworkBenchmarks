require "../models/*"

module TestController 
  class Plaintext < Kemalyst::Handler::Base
    def call(context)
      text "Hello, World!"
    end
  end

  class Json < Kemalyst::Handler::Base
    def call(context)
      results = {message: "Hello, World!"}
      json results.to_json
    end
  end

  class Db < Kemalyst::Handler::Base
    def call(context)
      results = {} of Symbol => Int32
      if world = World.find rand(1..10_000)
        results = {id: world.id, randomNumber: world.randomNumber}
      end
      json results.to_json
    end
  end

  class Queries < Kemalyst::Handler::Base
    def call(context)
      queries = context.params["queries"].as(String)
      queries = queries.to_i? || 1
      queries = queries.clamp(1..500)

      results = (1..queries).map do
        if world = World.find rand(1..10_000)
          {id: world.id, randomNumber: world.randomNumber}
        end
      end

      json results.to_json
    end
  end

  class Updates < Kemalyst::Handler::Base
    def call(context)
      queries = context.params["queries"].as(String)
      queries = queries.to_i? || 1
      queries = queries.clamp(1..500)

      updated = (1..queries).map do
        world = World.find rand(1..10_000)
        if world
          world.randomNumber = rand(1..10_000)
          world.save
          {id: world.id, randomNumber: world.randomNumber}
        end
      end

      json updated.to_json
    end
  end

  class Fortunes < Kemalyst::Handler::Base
    def call(context)
      fortune = Fortune.new
      fortune.id = 0
      fortune.message = "Additional fortune added at request time."

      fortunes = Fortune.all
      fortunes << fortune
      fortunes.sort_by! { |fortune| fortune.message.not_nil! }

      html render("fortune/index.slang", "main.slang")
    end
  end
end
