package com.typesafe.akka.http.benchmark.datastore

import java.sql.ResultSet
import java.util.Properties
import java.util.concurrent.Executors

import akka.actor.ActorSystem
import com.typesafe.akka.http.benchmark.entity.{Fortune, World}
import com.typesafe.config.Config
import org.apache.commons.dbcp2.{DriverManagerConnectionFactory, PoolableConnection, PoolableConnectionFactory, PoolingDataSource}
import org.apache.commons.pool2.impl.GenericObjectPool

import scala.concurrent.{ExecutionContext, Future, Promise}

class MySqlDataStore(components: {
  val system: ActorSystem
  val config: Config
}) extends DataStore {
  val config = components.config.getConfig("akka.http.benchmark.mysql")

  private val dataSource: PoolingDataSource[PoolableConnection] = {
    val jdbcUrl = config.getString("jdbc-url")
    val props = new Properties()
    props.setProperty("user", config.getString("dbuser"))
    props.setProperty("password", config.getString("dbpass"))

    val connectionFactory = new DriverManagerConnectionFactory(jdbcUrl, props)
    val poolableConnectionFactory = new PoolableConnectionFactory(connectionFactory, null)
    val connectionPool = new GenericObjectPool[PoolableConnection](poolableConnectionFactory)
    connectionPool.setTestOnBorrow(true)
    connectionPool.setMinIdle(config.getString("min-idle").toInt)
    connectionPool.setMaxIdle(config.getString("max-idle").toInt)
    connectionPool.setMaxTotal(config.getString("max-total").toInt)
    poolableConnectionFactory.setPool(connectionPool)
    poolableConnectionFactory.setValidationQuery("select 1")
    new PoolingDataSource[PoolableConnection](connectionPool)
  }
  private implicit val executionContext: ExecutionContext = {
    val size = config.getInt("thread-pool-size")
    val threadPool = Executors.newFixedThreadPool(size)
    new ExecutionContext {
      override def reportFailure(cause: Throwable): Unit = {
        components.system.log.error(cause, "exception in mysql thread pool")
      }

      override def execute(runnable: Runnable): Unit = {
        threadPool.execute(runnable)
      }
    }
  }

  override def findOne(id: Int): Future[World] = {
    val select = "select id, randomNumber from World where id = ?"
    val promise = Promise[World]()
    executionContext.execute(new Runnable {
      override def run(): Unit = {
        val conn = dataSource.getConnection
        val stmt = conn.prepareStatement(select)
        stmt.setInt(1, id)
        val rs = stmt.executeQuery()
        val world = rs.next() match {
          case true =>
            World(rs.getInt("id"), rs.getInt("randomNumber"))
        }
        rs.close()
        stmt.close()
        conn.close()
        promise.success(world)
      }
    })
    promise.future
  }

  override def updateOne(id: Int, randomNumber: Int): Future[Boolean] = {
    val update = "update World set randomNumber = ? where id = ?"
    val promise = Promise[Boolean]()
    executionContext.execute(new Runnable {
      override def run(): Unit = {
        val conn = dataSource.getConnection
        val stmt = conn.prepareStatement(update)
        stmt.setInt(1, randomNumber)
        stmt.setInt(2, id)
        val n = stmt.executeUpdate()
        stmt.close()
        conn.close()
        promise.success(n > 0)
      }
    })
    promise.future
  }

  override def getFortunes: Future[List[Fortune]] = {
    val select = "select id, message from Fortune"
    val promise = Promise[List[Fortune]]()
    executionContext.execute(new Runnable {
      override def run(): Unit = {
        val conn = dataSource.getConnection
        val stmt = conn.prepareStatement(select)
        val rs = stmt.executeQuery()
        val fortunes = {
          rs.map(r => Fortune(r.getInt("id"), r.getString("message"))).toList :+ Fortune(0, "Additional fortune added at request time.")
        }.sortBy(_.message)
        rs.close()
        stmt.close()
        conn.close()
        promise.success(fortunes)
      }
    })
    promise.future
  }

  implicit class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = rs.next()

    def next(): ResultSet = rs
  }

}
