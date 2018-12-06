class Benchmark < Application
  ID_MAXIMUM = 10_000

  base "/"
  before_action :get_query_count, only: [:queries, :updates]

  def get_query_count
    queries = query_params["queries"]
    queries = queries.to_i? || 1
    @queries = queries.clamp(1..500)
  end
  @queries : Int32 = 0

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
    if world = World.find rand(1..ID_MAXIMUM)
      results = {id: world.id, randomNumber: world.randomnumber}
    end

    render json: results
  end

  # Postgres Test 3: Multiple database query
  get "/queries", :queries do
    results = (1..@queries).map do
      if world = World.find rand(1..ID_MAXIMUM)
        {id: world.id, randomNumber: world.randomnumber}
      end
    end

    render json: results
  end

  # Postgres Test 5: Database Updates
  get "/updates", :updates do
    results = (1..@queries).map do
      if world = World.find rand(1..ID_MAXIMUM)
        world.randomnumber = rand(1..ID_MAXIMUM)
        world.save
        {id: world.id, randomNumber: world.randomnumber}
      end
    end

    render json: results
  end

  # Postgres Test 4: Fortunes
  get "/fortunes", :fortunes do
    fortune = Fortune.new
    fortune.id = 0
    fortune.message = "Additional fortune added at request time."

    fortunes = Fortune.all
    fortunes << fortune
    fortunes.sort_by! { |fortune| fortune.message || "" }

    # by default this would have been returned as text/html
    response.content_type = "text/html; charset=UTF-8"
    render html: Kilt.render("src/views/fortunes.ecr")
  end
end
