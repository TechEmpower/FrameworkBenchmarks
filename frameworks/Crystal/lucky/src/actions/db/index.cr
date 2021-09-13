class Db::Index < BaseAction
  get "/db" do
    world = World::BaseQuery.find(rand(1..ID_MAXIMUM))
    json WorldSerializer.new(world)
  end
end
