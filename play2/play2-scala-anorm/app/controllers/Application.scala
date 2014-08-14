package controllers

import play.api.Play.current
import play.api.db.DB
import play.api.mvc._
import play.api.libs.json.Json
import java.util.concurrent._
import scala.concurrent._
import models.{World, Fortune}
import utils._
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits._
import play.core.NamedThreadFactory

object Application extends Controller {

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

  // If the thread-pool used by the database grows too large then our server
  // is probably struggling, and we should start dropping requests. Set
  // the max size of our queue something above the number of concurrent
  // connections that we need to handle.
  def isDbQueueTooBig: Boolean = tpe.getQueue.size() <= 1024

  def db = PredicatedAction(isDbQueueTooBig, ServiceUnavailable) {
    Action.async {
      getRandomWorlds(1).map { worlds =>
        Ok(Json.toJson(worlds.head))
      }
    }
  }

  def queries(countString: String) = PredicatedAction(isDbQueueTooBig, ServiceUnavailable) {
    Action.async {
      val n = parseCount(countString)
      getRandomWorlds(n).map { worlds =>
        Ok(Json.toJson(worlds))
      }
    }
  }

  private def parseCount(s: String): Int = {
    try {
      val parsed = java.lang.Integer.parseInt(s, 10)
      parsed match {
        case i if i < 1 => 1
        case i if i > 500 => 500
        case i => i
      }
    } catch {
      case _: NumberFormatException => 1
    }
  }

  private def getRandomWorlds(n: Int): Future[Seq[World]] = Future {
    val random = ThreadLocalRandom.current()
    DB.withConnection { implicit connection =>
      for (_ <- 1 to n) yield {
        val randomId: Long = random.nextInt(TestDatabaseRows) + 1
        World.findById(randomId)
      }
    }
  }(dbEc)

  def fortunes() = PredicatedAction(isDbQueueTooBig, ServiceUnavailable) {
    Action.async {
      Future {
        val fortunes = Fortune.getAll()
        val extendedFortunes = Fortune(0.toLong, "Additional fortune added at request time.") +: fortunes
        Ok(views.html.fortune(extendedFortunes))
      }
    }
  }

  def update(countString: String) = PredicatedAction(isDbQueueTooBig, ServiceUnavailable) {
    Action.async {
      val n = parseCount(countString)
      Future {
        val random = ThreadLocalRandom.current()
        val worlds = DB.withConnection { implicit connection =>
          for(_ <- 1 to n) yield {
            val randomId: Long = random.nextInt(TestDatabaseRows) + 1
            val world = World.findById(random.nextInt(TestDatabaseRows) + 1)
            val updatedWorld = world.copy(randomNumber = random.nextInt(10000) + 1)
            World.updateRandom(updatedWorld)
            updatedWorld
          }
        }
        Ok(Json.toJson(worlds)).withHeaders("Server" -> "Netty")
      }(dbEc)
    }
  }
}