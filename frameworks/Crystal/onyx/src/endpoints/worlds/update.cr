struct Endpoints::Worlds::Update
  include Onyx::HTTP::Endpoint
  include RandomID

  QUERY = Models::World.update.set(random_number: 0).where(id: 0).build[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)

    worlds = Array(Models::World).new

    queries.clamp(1..500).times.each do
      id, number = {random_id, random_id}

      world = Models::World.new(id: id, random_number: number)
      worlds << world

      Onyx.exec(QUERY, world.random_number, world.id)
    end

    return Views::Worlds.new(worlds)
  end
end
