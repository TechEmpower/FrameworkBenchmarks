package controllers

import play.api.Play.current
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.concurrent.{Future, ExecutionContext}
import scala.collection.convert.WrapAsScala.collectionAsScalaIterable
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.concurrent.Akka

object Application extends Controller {

  val DEFAULT_HOST = "localhost:27017"
  val servers = current.configuration.getStringList("mongodb.servers") match {
    case Some(servers) => collectionAsScalaIterable(servers).toList
    case None => List(DEFAULT_HOST)
  }

  val DEFAULT_DB = "hello_world"
  val dbName = current.configuration.getString("mongodb.db").getOrElse(DEFAULT_DB)

  //private val dbExecutionContext: ExecutionContext = Akka.system.dispatchers.lookup("dbExecutionContext")
  private val database = {
    ReactiveMongoPlugin
      .driver
      .connection(servers, nbChannelsPerNode = 10)
      .db(dbName)//(dbExecutionContext)
  }

  private val projection = Json.obj("_id" -> 0)

  def getRandomWorlds(n: Int): Future[Seq[JsValue]] = {
    val random = ThreadLocalRandom.current()
    Future.sequence((for {
      _ <- 1 to n
    } yield {
      database.collection[JSONCollection]("world")
        .find(Json.obj("id" -> (random.nextInt(TestDatabaseRows) + 1)), projection)
        .one[JsValue].map(_.get)
    }))
  }

  // Common code between Scala database code

  private val TestDatabaseRows = 10000

  def db = Action.async {
    getRandomWorlds(1).map { worlds =>
      Ok(Json.toJson(worlds.head))
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds))
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
}