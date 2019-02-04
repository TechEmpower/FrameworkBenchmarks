package net.benchmark.akka.http.world

import net.benchmark.akka.http.db.CustomPostgresProfile
import net.benchmark.akka.http.db.CustomPostgresProfile.api._
import slick.basic.DatabaseConfig

import scala.concurrent.Future

class WorldRepositoryModule(val dbConfig: DatabaseConfig[CustomPostgresProfile]) extends WorldRepository {

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
