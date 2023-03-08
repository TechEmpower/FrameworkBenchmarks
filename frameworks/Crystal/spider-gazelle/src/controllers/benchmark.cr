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
    world = World.find(Random.rand(1..ID_MAXIMUM))
    results = {id: world.id, randomNumber: world.randomnumber}
    render json: results
  end

  # Postgres Test 3: Multiple database query
  get "/queries", :queries do
    results = (1..get_query_count).map do
      world = World.find(Random.rand(1..ID_MAXIMUM))
      {id: world.id, randomNumber: world.randomnumber}
    end
    render json: results
  end

  # Postgres Test 5: Database Updates
  get "/updates", :updates do
    results = (1..get_query_count).map do
      world = World.find(Random.rand(1..ID_MAXIMUM))
      random_number = Random.rand(1..ID_MAXIMUM)
      while random_number == world.randomnumber
        random_number = Random.rand(1..ID_MAXIMUM)
      end
      world.randomnumber = random_number
      world.save!
      {id: world.id, randomNumber: random_number}
    end

    render json: results
  end

  # Postgres Test 4: Fortunes
  FORTUNE_MESSAGE = "Additional fortune added at request time."
  FORTUNE_CTYPE   = "text/html; charset=UTF-8"

  get "/fortunes", :fortunes do
    fortunes = Fortune.all.to_a
    fortunes << Fortune.new(id: 0, message: FORTUNE_MESSAGE)
    fortunes.sort_by!(&.message)

    # by default this would have been returned as text/html
    response.content_type = FORTUNE_CTYPE
    render template: "fortunes.ecr"
  end
end
