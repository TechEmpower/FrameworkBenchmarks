struct Endpoints::Worlds::Many
  include Onyx::HTTP::Endpoint
  include RandomID

  QUERY = Models::World.where("id = ANY(?)", 0).build(true)[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)
    ids = queries.clamp(1..500).times.map do
      random_id
    end.join(',').try do |s|
      "{#{s}}"
    end

    worlds = Onyx.query(Models::World, QUERY, ids)
    return Views::Worlds.new(worlds)
  end
end
