class Benchmark < Application
  ID_MAXIMUM = 10_000

  base "/"

  def get_query_count
    queries = query_params["queries"]
    queries = queries.to_i? || 1
    queries.clamp(1..500)
  end

  # Test 1: JSON Serialization
  get "/json", :json do
    render json: {message: "Hello, World!"}
  end

  # Test 6: Plaintext
  get "/plaintext", :plaintext do
    render text: "Hello, World!"
  end

  # Postgres Test 2: Single database query
  get "/db", :db do
    results = {} of Symbol => Int32
    if world = World.find(Random.rand(ID_MAXIMUM).succ)
      results = {id: world.id, randomNumber: world.randomnumber}
    end

    render json: results
  end

  # Postgres Test 3: Multiple database query
  get "/queries", :queries do
    results = (1..get_query_count).map do
      if world = World.find(Random.rand(ID_MAXIMUM).succ)
        {id: world.id, randomNumber: world.randomnumber}
      end
    end

    render json: results
  end

  # Postgres Test 5: Database Updates
  get "/updates", :updates do
    results = (1..get_query_count).map do
      if world = World.find(Random.rand(ID_MAXIMUM).succ)
        world.randomnumber = Random.rand(ID_MAXIMUM).succ
        world.save
        {id: world.id, randomNumber: world.randomnumber}
      end
    end

    render json: results
  end

  # Postgres Test 4: Fortunes
  FORTUNE_MESSAGE = "Additional fortune added at request time."
  FORTUNE_CTYPE = "text/html; charset=UTF-8"

  get "/fortunes", :fortunes do
    fortune = Fortune.new
    fortune.id = 0
    fortune.message = FORTUNE_MESSAGE

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message || "" }

    # by default this would have been returned as text/html
    response.content_type = FORTUNE_CTYPE
    render template: "fortunes.ecr"
  end
end
