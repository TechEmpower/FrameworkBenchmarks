struct Endpoints::Worlds::Many
  include Onyx::HTTP::Endpoint
  include RandomID

  QUERY = Models::World.where(id: 0).build(true)[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)

    worlds = Array(Models::World).new

    queries.clamp(1..500).times.each do
      world = Onyx.query(Models::World, QUERY, random_id).first
      worlds << world
    end

    return Views::Worlds.new(worlds)
  end
end
