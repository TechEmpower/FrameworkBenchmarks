package controllers

import java.util.concurrent.ThreadLocalRandom
import javax.inject.{Singleton, Inject}

import play.api.libs.json.{JsObject, Json, JsValue}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import reactivemongo.api.ReadPreference

import scala.concurrent.Future

import play.api.mvc.{ Action, Controller }

import play.modules.reactivemongo.{
MongoController, ReactiveMongoApi, ReactiveMongoComponents
}
import play.modules.reactivemongo.json._
import play.modules.reactivemongo.json.collection.JSONCollection

@Singleton
class Application @Inject() (val reactiveMongoApi: ReactiveMongoApi)
  extends Controller with MongoController with ReactiveMongoComponents {

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

  def updateWorlds(queries: Int): Future[Seq[Option[JsObject]]] = {
    val futureWorlds: Future[Seq[Option[JsObject]]] = getRandomWorlds(queries)
    val futureNewWorlds: Future[Seq[Option[JsObject]]] = futureWorlds.map( worlds => {
      worlds.map(worldOption => {
        worldOption.map(world => {
          val newWorld = world ++ Json.obj("randomNumber" -> getNextRandom)
          worldCollection.update(world, newWorld)
          newWorld
        })
      })
    })
    futureNewWorlds
  }

  def getNextRandom: Int = {
    ThreadLocalRandom.current().nextInt(TestDatabaseRows) + 1
  }

  // Test seems picky about headers.  Doesn't like character set being there for JSON.  Always wants Server header set.
  // There is a Filter which adds the Server header for all types.  Below I set Content-Type as needed to get rid of
  // warnings.

  // Easy ones
  case class HelloWorld(message: String)

  def getJsonMessage = Action {
    val helloWorld = HelloWorld(message = "Hello, World!")
    Ok(Json.toJson(helloWorld)(Json.writes[HelloWorld])).withHeaders(CONTENT_TYPE -> "application/json")
  }

  val plaintext = Action {
    // default headers are correct according to docs: charset included.
    // BUT the test harness has a WARN state and says we don't need it.
    Ok("Hello, World!").withHeaders(CONTENT_TYPE -> "text/plain")
  }

  // Semi-Common code between Scala database code

  protected val TestDatabaseRows = 10000

  def doDb = Action.async {
    getRandomWorld.map { worlds =>
      Ok(Json.toJson(worlds.head)).withHeaders(CONTENT_TYPE -> "application/json")
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(CONTENT_TYPE -> "application/json")
    }
  }

  private def byMessage(item: JsValue): String = {
    (item \ "message").as[String]
  }

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes =  Json.obj("_id" -> 0, "message" -> "Additional fortune added at request time.") :: dbFortunes

      val sorted = appendedFortunes.sortBy(byMessage(_))

      Ok(views.html.fortune(sorted)).withHeaders(CONTENT_TYPE -> "text/html")
    }
  }

  def update(queries: String) = Action.async {
    val n = parseCount(queries)
    updateWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(CONTENT_TYPE -> "application/json")
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
