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

  private val TestDatabaseRows = 10000

  val DEFAULT_HOST = "localhost:27017"
  val servers = current.configuration.getStringList("mongodb.servers") match {
    case Some(servers) => collectionAsScalaIterable(servers).toList
    case None => List(DEFAULT_HOST)
  }

  val DEFAULT_DB = "hello_world"
  val db = current.configuration.getString("mongodb.db").getOrElse(DEFAULT_DB)

  //private val dbExecutionContext: ExecutionContext = Akka.system.dispatchers.lookup("dbExecutionContext")
  private val database = ReactiveMongoPlugin
    .driver
    .connection(servers, nbChannelsPerNode = 10)
    .db(db)//(dbExecutionContext)

  private def collection: JSONCollection = database.collection[JSONCollection]("world")
  private val projection = Json.obj("_id" -> 0)

  def db(queries: Int) = Action {
    import scala.concurrent.ExecutionContext.Implicits.global

    Async {
      val random = ThreadLocalRandom.current()
      val futureWorlds = Future.sequence((for {
        _ <- 1 to queries
      } yield { collection
        .find(Json.obj("id" -> (random.nextInt(TestDatabaseRows) + 1)), projection)
        .cursor[JsValue]
        .toList.map(list => list.head)
      }))

      futureWorlds.map { worlds =>
        Ok(Json.toJson(worlds))
      }
    }
  }

}