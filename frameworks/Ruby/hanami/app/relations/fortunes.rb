module HelloWorld
  module Relations
    class Fortunes < HelloWorld::DB::Relation
      schema :Fortune, infer: true, as: :fortunes
    end
  end
end
