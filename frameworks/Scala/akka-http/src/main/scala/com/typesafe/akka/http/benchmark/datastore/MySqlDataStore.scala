package com.typesafe.akka.http.benchmark.datastore

import java.sql.PreparedStatement
import java.sql.ResultSet
import java.util.concurrent.Executors

import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.entity.Fortune
import com.typesafe.akka.http.benchmark.entity.World
import com.zaxxer.hikari._

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

// TODO: use slick or similar here instead for more idiomatic usage
trait MySqlDataStore extends DataStore { _: Infrastructure =>
  lazy val config = appConfig.getConfig("akka.http.benchmark.mysql")

  private lazy val dataSource = new HikariDataSource {
    setJdbcUrl(config.getString("jdbc-url"))
    setUsername(config.getString("dbuser"))
    setPassword(config.getString("dbpass"))
    setMaximumPoolSize(config.getInt("connection-pool-size"))
  }

  private implicit lazy val dbExecutionContext: ExecutionContext = {
    val size = config.getInt("thread-pool-size")
    val threadPool = Executors.newFixedThreadPool(size)
    ExecutionContext.fromExecutor(threadPool)
  }

  def requireWorldById(id: Int): Future[World] = findWorldById(id).map(_.getOrElse(throw new RuntimeException(s"Element with id $id was not found.")))(executionContext)
  override def findWorldById(id: Int): Future[Option[World]] =
    withStatement("select id, randomNumber from World where id = ?") { stmt =>
      stmt.setInt(1, id)
      val rs = stmt.executeQuery()

      if (rs.next()) Some(World(rs.getInt("id"), rs.getInt("randomNumber")))
      else None
    }

  override def updateWorld(world: World): Future[Boolean] =
    withStatement("update World set randomNumber = ? where id = ?") { stmt =>
      stmt.setInt(1, world.randomNumber)
      stmt.setInt(2, world.id)
      stmt.executeUpdate() > 0
    }

  override def getFortunes: Future[immutable.Seq[Fortune]] =
    withStatement("select id, message from Fortune") { stmt =>
      val rs = stmt.executeQuery()
      (Fortune(0, "Additional fortune added at request time.") +: rs.map(r => Fortune(r.getInt("id"), r.getString("message"))).toVector)
        .sortBy(_.message)
    }

  private def withStatement[T](statement: String)(f: PreparedStatement => T): Future[T] =
    Future {
      val conn = dataSource.getConnection
      val stmt = conn.prepareStatement(statement)
      try f(stmt)
      finally {
        stmt.close()
        conn.close()
      }
    }(dbExecutionContext)

  implicit class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = rs.next()
    def next(): ResultSet = rs
  }
}
