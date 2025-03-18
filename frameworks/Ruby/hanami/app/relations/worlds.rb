module HelloWorld
  module Relations
    class Worlds < HelloWorld::DB::Relation
      schema :World, infer: true, as: :worlds
    end
  end
end
