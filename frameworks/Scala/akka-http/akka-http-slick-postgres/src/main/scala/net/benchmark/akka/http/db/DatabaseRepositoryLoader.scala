package net.benchmark.akka.http.db

import net.benchmark.akka.http.fortune.{FortuneRepository, FortuneRepositoryModule}
import net.benchmark.akka.http.world.{WorldRepository, WorldRepositoryModule}
import slick.basic.DatabaseConfig
import slick.jdbc.PostgresProfile

trait DatabaseRepositoryLoader {

  def close(): Unit

  def loadWorldRepository(): WorldRepository

  def loadFortuneRepository(): FortuneRepository

}

final class DatabaseRepositoryLoaderModule(dbConfig: DatabaseConfig[PostgresProfile]) extends DatabaseRepositoryLoader {

  override def close(): Unit = dbConfig.db.close()

  override def loadWorldRepository(): WorldRepository = new WorldRepositoryModule(dbConfig)

  override def loadFortuneRepository(): FortuneRepository = new FortuneRepositoryModule(dbConfig)

}
