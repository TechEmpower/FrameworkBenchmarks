package net.benchmark.akka.http.fortune

import net.benchmark.akka.http.db.CustomPostgresProfile
import net.benchmark.akka.http.db.CustomPostgresProfile.api._
import slick.basic.{DatabaseConfig, DatabasePublisher}
import slick.jdbc.{ResultSetConcurrency, ResultSetType}

class FortuneRepositoryModule(val dbConfig: DatabaseConfig[CustomPostgresProfile]) extends FortuneRepository {

  private val db = dbConfig.db

  private val fortunes = FortuneTable.fortuneTableQuery

  override def all(): DatabasePublisher[Fortune] = {
    db.stream(
      fortunes.result.withStatementParameters(rsType = ResultSetType.ForwardOnly,
                                              rsConcurrency = ResultSetConcurrency.ReadOnly,
                                              fetchSize = 100))
  }

}
