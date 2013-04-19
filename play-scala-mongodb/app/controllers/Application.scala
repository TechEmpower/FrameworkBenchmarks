package controllers

import play.api.Play.current
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.concurrent.Future
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {

  private val TestDatabaseRows = 10000
  private val database = ReactiveMongoPlugin.db
  private def collection: JSONCollection = database.collection[JSONCollection]("world")
  private val worldWithoutMongoId = (__ \ "_id").json.prune

  def db(queries: Int) = Action {
    import scala.concurrent.ExecutionContext.Implicits.global

    Async {
      val random = ThreadLocalRandom.current()
      val futureWorlds = Future.sequence((for {
        _ <- 1 to queries
      } yield { collection
        .find(Json.obj("id" -> (random.nextInt(TestDatabaseRows) + 1)))
        .cursor[JsValue]
        .toList map {
          l => l.head.transform(worldWithoutMongoId).get
        }
      }))

      futureWorlds.map { worlds =>
        Ok(Json.toJson(worlds))
      }
    }
  }

}