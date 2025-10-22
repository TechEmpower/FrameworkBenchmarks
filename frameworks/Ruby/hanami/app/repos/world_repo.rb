module HelloWorld
  module Repos
    class WorldRepo < HelloWorld::DB::Repo
      QUERY_RANGE = 1..10_000    # range of IDs in the Fortune DB

      def find(id)
        worlds.by_pk(id).one
      end

      def update_random_number(id)
        world = find(id)
        world_hash = world.to_h
        new_value = random_id
        new_value = random_id while new_value == world_hash[:randomnumber]
        worlds.where(id: id).update({ randomnumber: new_value })
        world_hash
      end

      private

        def random_id
          Random.rand(QUERY_RANGE)
        end
    end
  end
end
