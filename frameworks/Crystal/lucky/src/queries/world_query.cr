class WorldQuery < World::BaseQuery
  def random_world(id)
    random_number = LuckyRecord::Repo.run do |db|
      db.query_all "SELECT randomNumber FROM world WHERE id = $1", id, as: Int32
    end
    random_number
  end

  def set_world(world)
    LuckyRecord::Repo.run do |db|
      db.exec("UPDATE world SET randomNumber = $1 WHERE id = $2", world[:randomNumber], world[:id])
    end
    world
  end
end
