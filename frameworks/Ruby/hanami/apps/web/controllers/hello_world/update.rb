module Web::Controllers::HelloWorld
  class Update
    include Web::Action

    def call(params)
      queries = (params[:queries] || 1).to_i
      queries = 1 if queries < 1
      queries = 500 if queries > 500

      repository = WorldRepository.new
      worlds = (1..queries).map do
        world = repository.find_random_entity
        world = repository.update(world.id, randomNumber: Random.rand(10000) + 1)
        world.to_h
      end
      status 200, worlds.to_json
    end
  end
end
