package controllers

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc._
import reactivemongo.api.{Cursor, ReadPreference}
import reactivemongo.play.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json._

class Application (val controllerComponents: ControllerComponents, reactiveMongoApi: ReactiveMongoApi)(implicit ec: ExecutionContext)
  extends BaseController {

  private def worldCollection: Future[JSONCollection] = reactiveMongoApi.database.map(_.collection[JSONCollection]("world"))
  private def fortuneCollection: Future[JSONCollection] = reactiveMongoApi.database.map(_.collection[JSONCollection]("fortune"))
  private val projection = Json.obj("_id" -> 0)

  def getRandomWorlds(queries: Int): Future[Seq[Option[JsObject]]] = {
    val futureWorlds: Seq[Future[Option[JsObject]]] = for {
      _ <- 1 to queries
    } yield { worldCollection.map(_
      .find(Json.obj("_id" -> getNextRandom), Option(projection))
      .one[JsObject]).flatten
    }
    Future.sequence(futureWorlds)
  }

  def getRandomWorld = {
    worldCollection.map(_
      .find(Json.obj("id" -> getNextRandom), Option(projection))
      .one[JsValue]).flatten
  }

  def getFortunes: Future[List[JsObject]] = {
      fortuneCollection.map(_.find(Json.obj(), Option.empty[JsObject])
        .cursor[JsObject](ReadPreference.primaryPreferred, false).collect[List](Int.MaxValue, (v, t) => Cursor.Fail(t))).flatten
  }

  def updateWorlds(queries: Int): Future[Seq[JsObject]] = {
    getRandomWorlds(queries)
      .map(_.flatten)
      .map(_.map(oldWorld => {
        val newWorld = oldWorld ++ Json.obj("randomNumber" -> getNextRandom)
        worldCollection.map(_.update(oldWorld, newWorld).map(result => newWorld)).flatten
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
