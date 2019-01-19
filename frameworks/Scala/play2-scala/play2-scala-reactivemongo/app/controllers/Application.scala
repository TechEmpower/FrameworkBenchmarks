package controllers

import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.{Future, ExecutionContext}

import play.api.libs.json.{JsObject, Json, JsValue}
import play.api.mvc._
import play.mvc.Http

import reactivemongo.api.ReadPreference
import reactivemongo.play.json.collection.JSONCollection
import play.modules.reactivemongo.{
  ReactiveMongoApi, ReactiveMongoComponents, MongoController
}
import play.modules.reactivemongo.json._

class Application (val controllerComponents: ControllerComponents, reactiveMongoApi: ReactiveMongoApi)(implicit ec: ExecutionContext)
  extends BaseController {

  private def worldCollection: JSONCollection = reactiveMongoApi.db.collection[JSONCollection]("world")
  private def fortuneCollection: JSONCollection = reactiveMongoApi.db.collection[JSONCollection]("fortune")
  private val projection = Json.obj("_id" -> 0)

  def getRandomWorlds(queries: Int): Future[Seq[Option[JsObject]]] = {
    val futureWorlds: Seq[Future[Option[JsObject]]] = for {
      _ <- 1 to queries
    } yield { worldCollection
      .find(Json.obj("_id" -> getNextRandom), projection)
      .one[JsObject]
    }
    Future.sequence(futureWorlds)
  }

  def getRandomWorld = {
    val futureWorld = worldCollection
      .find(Json.obj("id" -> getNextRandom), projection)
      .one[JsValue]
    futureWorld
  }

  def getFortunes: Future[List[JsObject]] = {
    val futureFortunes: Future[List[JsObject]] =
      fortuneCollection.find(Json.obj())
        .cursor[JsObject](ReadPreference.primaryPreferred, false).collect[List]()
    futureFortunes
  }

  def updateWorlds(queries: Int): Future[Seq[JsObject]] = {
    getRandomWorlds(queries)
      .map(_.flatten)
      .map(_.map(oldWorld => {
        val newWorld = oldWorld ++ Json.obj("randomNumber" -> getNextRandom)
        worldCollection.update(oldWorld, newWorld).map(result => newWorld)
      }))
      .map(Future.sequence(_))
      .flatten
  }

  def getNextRandom: Int = {
    ThreadLocalRandom.current().nextInt(TestDatabaseRows) + 1
  }

  // Semi-Common code between Scala database code

  protected val TestDatabaseRows = 10000

  def db = Action.async {
    getRandomWorld.map { worlds =>
      Ok(Json.toJson(worlds.head))
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds))
    }
  }

  private def byMessage(item: JsValue): String = {
    (item \ "message").as[String]
  }

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes =  Json.obj("_id" -> 0, "message" -> "Additional fortune added at request time.") :: dbFortunes

      val sorted = appendedFortunes.sortBy(byMessage(_))

      Ok(views.html.fortune(sorted))
    }
  }

  def update(queries: String) = Action.async {
    val n = parseCount(queries)
    updateWorlds(n).map { worlds =>
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
