class Updates::Index < BaseAction
  include QueriesParser

  get "/updates" do
    results = (1..queries_param).map do
      world = World::BaseQuery.find(rand(1..ID_MAXIMUM))
      # Avram does not perform the update operation if no value has changed
      # so we have to keep generating random numbers until we
      # get one that is different than the currently set one
      random_number = rand(1..ID_MAXIMUM)
      while random_number == world.randomnumber
        random_number = rand(1..ID_MAXIMUM)
      end
      world = World::SaveOperation.update!(world, randomnumber: random_number)
      WorldSerializer.new(world)
    end

    json results
  end
end
