class WorldRepository < Hanami::Repository
  self.relation = :world

  def find_random_entity
    find(Random.rand(10000) + 1)
  end
end
