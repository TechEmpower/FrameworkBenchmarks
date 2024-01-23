# frozen_string_literal: true

module HelloWorld
  module Actions
    module Db
      class Index < HelloWorld::Action
        QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB
        include Deps["persistence.rom"]

        def handle(*, response)
          world = rom.relations[:World].where(id: random_id).one
          response.headers['Server'] = 'hanami'
          response.headers['Date'] = Time.now.httpdate
          response.format = :json
          response.body = world.to_json
        end

        def random_id
          Random.rand(QUERY_RANGE)
        end
      end
    end
  end
end
