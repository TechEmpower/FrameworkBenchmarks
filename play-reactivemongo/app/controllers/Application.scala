package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent._
import play.api.Play.current
import play.api.Logger
import play.modules.reactivemongo._
import play.modules.reactivemongo.PlayBsonImplicits._
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.bson.handlers.DefaultBSONHandlers._
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent._

object Application extends Controller with MongoController {
  
  val TEST_DATABASE_ROWS = 10000
  val database = ReactiveMongoPlugin.db
  lazy val worlds = database("worlds")

  def json() = Action {
    Ok(Json.obj("message" -> "Hello World!"))   
  }

  def db(queries: Int) = Action {
    import play.api.libs.iteratee.Iteratee
    import scala.concurrent.ExecutionContext.Implicits.global

    val random = ThreadLocalRandom.current()

    Async {
      val futureWorlds = (1 to queries) map { _ =>
        worlds.find[JsValue]( QueryBuilder().query( Json.obj(
          "id" -> (random.nextInt(TEST_DATABASE_ROWS) + 1) 
        ))).toList
      }

      futureWorlds.head map {
        fW => Ok(Json.toJson(fW))
      }
    }
  }
}