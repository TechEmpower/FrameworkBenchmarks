package utils

import java.sql.Connection
import java.util.concurrent._
import javax.inject.{Singleton, Inject}
import play.api.db.Database
import play.api.Play.current
import play.core.NamedThreadFactory
import play.db.NamedDatabase
import scala.concurrent._
import scala.concurrent.Future

@Singleton
class DbOperation @Inject()(@NamedDatabase("hello_world") protected val db: Database) {

  private val maxDbOperations = current.configuration.underlying.getInt("max-db-ops")

  private val partitionCount = current.configuration.getInt("db.hello_world.partitionCount").getOrElse(2)
  private val maxConnections =
    partitionCount * current.configuration.getInt("db.hello_world.maxConnectionsPerPartition").getOrElse(5)
  private val minConnections =
    partitionCount * current.configuration.getInt("db.hello_world.minConnectionsPerPartition").getOrElse(5)

  private val tpe = new ThreadPoolExecutor(minConnections, maxConnections,
    0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable](), // TODO: Could use ArrayBlockingQueue?
    new NamedThreadFactory("dbEc"))
  private val dbEc = ExecutionContext.fromExecutorService(tpe)

  // Anorm code

  /**
   * Run a DB operation in the DB context. Automatically
   * provides a Session.
   */
  def asyncDbOp[T](op: Connection => T): Future[T] = {
    // If the thread-pool queue used by the database grows too large then our server
    // is probably struggling, and we should start dropping requests. If we don't
    // then we'll just slow everything down and it will fail anyway. Better to fail
    // quickly rather than slowly. Set the max size of our queue something above the
    // number of concurrent connections that we expect to be handling.
    if (tpe.getQueue.size > maxDbOperations) sys.error(s"Aborted DB operation because queue is > $maxDbOperations")
    Future {
      db.withConnection { connection => op(connection) }
    }(dbEc)
  }

}