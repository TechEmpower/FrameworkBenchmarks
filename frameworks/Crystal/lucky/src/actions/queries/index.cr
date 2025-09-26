class Queries::Index < BaseAction
  include QueriesParser

  get "/queries" do
    results = (1..queries_param).map do
      world = World::BaseQuery.find(rand(1..ID_MAXIMUM))
      WorldSerializer.new(world)
    end

    json results
  end
end
