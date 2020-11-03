class Updates::Index < BaseAction
  include QueriesParser

  get "/updates" do
    results = (1..queries_param).map do
      world = WorldQuery.find(rand(1..ID_MAXIMUM))
      world = SaveWorld.update!(world, randomnumber: rand(1..ID_MAXIMUM))
      WorldSerializer.new(world)
    end

    json results
  end
end
