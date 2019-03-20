package utils

import akka.actor.ActorSystem
import java.sql.Connection
import java.util.concurrent._
import javax.inject.{Singleton, Inject}
import play.api.db.Database
import play.api.libs.concurrent.CustomExecutionContext
import play.api.Configuration
import scala.concurrent._
import scala.concurrent.Future

@Singleton
class DbOperation @Inject() (protected val db: Database,
  configuration: Configuration) {

  /**
   * Run a DB operation in the DB context.
   */
  def syncDbOp[T](op: Connection => T): T = {
    // If the thread-pool queue used by the database grows too large then our server
    // is probably struggling, and we should start dropping requests. If we don't
    // then we'll just slow everything down and it will fail anyway. Better to fail
    // quickly rather than slowly. Set the max size of our queue something above the
    // number of concurrent connections that we expect to be handling.
    db.withConnection { connection => op(connection) }
  }

}

class DatabaseExecutionContext @Inject()(actorSystem: ActorSystem)
 extends CustomExecutionContext(actorSystem, "database.dispatcher")
