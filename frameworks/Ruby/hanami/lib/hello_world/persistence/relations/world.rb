module HelloWorld
  module Persistence
    module Relations
      class World < ROM::Relation[:sql]
        schema(:World, infer: true)
      end
    end
  end
end
