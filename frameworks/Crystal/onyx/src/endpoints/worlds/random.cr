struct Endpoints::Worlds::Random
  include Onyx::HTTP::Endpoint
  include RandomID

  QUERY = Models::World.where(id: 0).build(true)[0]

  def call
    world = Onyx.query(Models::World, QUERY, random_id).first
    return Views::World.new(world)
  end
end
