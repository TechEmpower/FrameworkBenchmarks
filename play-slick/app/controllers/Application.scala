package controllers

import play.api.Play.current
import play.api.mvc._
import play.api.libs.json.Json
import java.util.concurrent._
import scala.concurrent._
import models.{Worlds, World, Fortunes, Fortune}
import utils._
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits._
import play.core.NamedThreadFactory

object Application extends Controller {

  private val MaxQueriesPerRequest = 20
  private val TestDatabaseRows = 10000

  private val partitionCount = current.configuration.getInt("db.default.partitionCount").getOrElse(2)
  private val maxConnections =
    partitionCount * current.configuration.getInt("db.default.maxConnectionsPerPartition").getOrElse(5)
  private val minConnections =
    partitionCount * current.configuration.getInt("db.default.minConnectionsPerPartition").getOrElse(5)

  private val tpe = new ThreadPoolExecutor(minConnections, maxConnections,
    0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable](),
    new NamedThreadFactory("dbEc"))
  private val dbEc = ExecutionContext.fromExecutorService(tpe)

  private val worldsTable = new Worlds
  private val fortunesTable = new Fortunes

  // A predicate for checking our ability to service database requests is determined by ensuring that the request
  // queue doesn't fill up beyond a certain threshold. For convenience we use the max number of connections * the max
  // # of db requests per web request to determine this threshold. It is a rough check as we don't know how many
  // queries we're going to make or what other threads are running in parallel etc. Nevertheless, the check is
  // adequate in order to throttle the acceptance of requests to the size of the pool.
  def isDbAvailable: Boolean = (tpe.getQueue.size() < maxConnections * MaxQueriesPerRequest)

  def db(queries: Int) = PredicatedAction(isDbAvailable, ServiceUnavailable) {
    Action {
      Async {
        val random = ThreadLocalRandom.current()

        val _worlds = Future.sequence((for {
          _ <- 1 to queries
        } yield Future(worldsTable.findById(random.nextInt(TestDatabaseRows) + 1))(dbEc)
          ).toList)

        _worlds map {
          w => Ok(Json.toJson(w))
        }
      }
    }
  }

  def fortunes() = PredicatedAction(isDbAvailable, ServiceUnavailable) {
    Action {
      Async {
        Future(fortunesTable.getAll())(dbEc).map { fs =>
          val fortunes =  Fortune(-1, "Additional fortune added at request time.") +: fs
          Ok(views.html.fortune(fortunes))
        }
      }
    }
  }

  def update(queries: Int) = PredicatedAction(isDbAvailable, ServiceUnavailable) {
    Action {
      Async {
        val random = ThreadLocalRandom.current()

        val boundsCheckedQueries = queries match {
          case q if q > 500 => 500
          case q if q <   1 => 1
          case _ => queries
        }

        val worlds = Future.sequence((for {
          _ <- 1 to boundsCheckedQueries
        } yield Future {
            for {
              world <- worldsTable.findById(random.nextInt(TestDatabaseRows) + 1) 
            } yield {
              val updatedWorld = world.copy(randomNumber = random.nextInt(TestDatabaseRows) + 1)
              worldsTable.updateRandom(updatedWorld)
              updatedWorld
            }
          }(dbEc)
        ).toList)

        worlds.map {
          w => Ok(Json.toJson(w)).withHeaders("Server" -> "Netty")
        }
      }
    }
  }
}