package utils

import java.sql.Connection
import java.util.concurrent._
import play.api.db.DB
import play.api.Play.current
import play.core.NamedThreadFactory
import scala.concurrent._
import scala.concurrent.Future

object DbOperation {

  // Common code between Anorm and Slick

  private val maxDbOperations = current.configuration.underlying.getInt("max-db-ops")

  private val partitionCount = current.configuration.getInt("db.default.partitionCount").getOrElse(2)
  private val maxConnections =
    partitionCount * current.configuration.getInt("db.default.maxConnectionsPerPartition").getOrElse(5)
  private val minConnections =
    partitionCount * current.configuration.getInt("db.default.minConnectionsPerPartition").getOrElse(5)

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
      DB.withConnection { connection => op(connection) }
    }(dbEc)
  }

}