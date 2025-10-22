# frozen_string_literal: true

module HelloWorld
  module Actions
    module DB
      class Index < HelloWorld::Action
        QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB

        include Deps["repos.world_repo"]

        def handle(*, response)
          world = world_repo.find(random_id)
          response.format = :json
          response.body = world.to_h.to_json
        end

        def random_id
          Random.rand(QUERY_RANGE)
        end
      end
    end
  end
end
