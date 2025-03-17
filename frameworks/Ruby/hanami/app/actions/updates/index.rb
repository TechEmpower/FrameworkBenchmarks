# frozen_string_literal: true

module HelloWorld
  module Actions
    module Updates
      class Index < HelloWorld::Action
        QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB
        ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
        MIN_QUERIES = 1            # min number of records that can be retrieved
        MAX_QUERIES = 500          # max number of records that can be retrieved

        include Deps["persistence.rom"]

        def handle(request, response)
          worlds = ALL_IDS.sample(queries(request)).map do |id|
            world = rom.relations[:World].by_pk(id)
            world_struct = world.one
            new_value = random_id
            new_value = random_id while new_value == world_struct[:randomnumber]
            world_struct[:randomnumber] = new_value
            world.command(:update).call(randomnumber: world_struct[:randomnumber])
            world_struct
          end
          response.headers['Server'] = 'hanami'
          response.headers['Date'] = Time.now.httpdate
          response.format = :json
          response.body = worlds.to_json
        end

        private

        def queries(request)
          request.params[:queries].to_i.clamp(MIN_QUERIES, MAX_QUERIES)
        end

        def random_id
          Random.rand(QUERY_RANGE)
        end
      end
    end
  end
end
