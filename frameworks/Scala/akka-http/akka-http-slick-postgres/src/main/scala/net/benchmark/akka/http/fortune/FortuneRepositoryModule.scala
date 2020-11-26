package net.benchmark.akka.http.fortune

import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.basic.{DatabaseConfig, DatabasePublisher}
import slick.jdbc.{ResultSetConcurrency, ResultSetType}

class FortuneRepositoryModule(val dbConfig: DatabaseConfig[PostgresProfile]) extends FortuneRepository {

  private val db = dbConfig.db

  private val fortunes = FortuneTable.fortuneTableQuery

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  override def all(): DatabasePublisher[Fortune] = {
    db.stream(
      fortunes.result.withStatementParameters(rsType = ResultSetType.ForwardOnly,
                                              rsConcurrency = ResultSetConcurrency.ReadOnly,
                                              fetchSize = 100))
  }

}
