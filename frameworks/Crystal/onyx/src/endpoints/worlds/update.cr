struct Endpoints::Worlds::Update
  include Onyx::HTTP::Endpoint
  include RandomID

  SELECT_QUERY = Models::World.where(id: 0).build(true)[0]
  UPDATE_QUERY = Models::World.update.set(random_number: 0).where(id: 0).build[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)

    worlds = Array(Models::World).new

    queries.clamp(1..500).times.each do
      world = Onyx.query(Models::World, SELECT_QUERY, random_id).first
      world.random_number = random_id
      Onyx.exec(UPDATE_QUERY, world.random_number, world.id)
      worlds << world
    end

    return Views::Worlds.new(worlds)
  end
end
