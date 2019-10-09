package net.benchmark.akka.http.world

import slick.basic.DatabaseConfig
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

class WorldRepositoryModule(val dbConfig: DatabaseConfig[PostgresProfile]) extends WorldRepository {

  private val db = dbConfig.db

  private val worlds = WorldTable.worldTableQuery

  override def find(id: Int): Future[Option[World]] = {
    db.run(worlds.filter(_.id === id).result.headOption)
  }

  override def require(id: Int): Future[World] = {
    db.run(worlds.filter(_.id === id).result.head)
  }

  override def update(world: World): Future[Int] = {
    db.run(worlds.filter(_.id === world.id).map(_.randomNumber).update(world.randomNumber))
  }

}
