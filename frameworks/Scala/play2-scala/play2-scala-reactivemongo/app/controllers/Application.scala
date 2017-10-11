package controllers

import java.util.concurrent.ThreadLocalRandom
import javax.inject.{Singleton, Inject}

import play.api.libs.json.{JsObject, Json, JsValue}
import reactivemongo.api.ReadPreference

import scala.concurrent.Future

import play.api.mvc.{ Action, BaseController, ControllerComponents, PlayBodyParsers }
import play.mvc.Http

import play.modules.reactivemongo.{
MongoController, ReactiveMongoApi, ReactiveMongoComponents
}
import play.modules.reactivemongo.json._
import reactivemongo.play.json.collection.JSONCollection
import scala.concurrent.ExecutionContext

@Singleton
class Application @Inject() (val reactiveMongoApi: ReactiveMongoApi, val controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
  extends BaseController with MongoController with ReactiveMongoComponents {

  val defaultHeader = Http.HeaderNames.SERVER -> "Play Framework"

  override lazy val parse: PlayBodyParsers = parse

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

  case class HelloWorld(message: String)

  def getJsonMessage = Action {
    val helloWorld = HelloWorld(message = "Hello, World!")
    Ok(Json.toJson(helloWorld)(Json.writes[HelloWorld])).withHeaders(defaultHeader)
  }

  val plaintext = Action {
    Ok("Hello, World!").withHeaders(defaultHeader).as("text/plain")
  }

  // Semi-Common code between Scala database code

  protected val TestDatabaseRows = 10000

  def doDb = Action.async {
    getRandomWorld.map { worlds =>
      Ok(Json.toJson(worlds.head)).withHeaders(defaultHeader)
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(defaultHeader)
    }
  }

  private def byMessage(item: JsValue): String = {
    (item \ "message").as[String]
  }

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes =  Json.obj("_id" -> 0, "message" -> "Additional fortune added at request time.") :: dbFortunes

      val sorted = appendedFortunes.sortBy(byMessage(_))

      Ok(views.html.fortune(sorted)).withHeaders(defaultHeader).as(HTML)
    }
  }

  def update(queries: String) = Action.async {
    val n = parseCount(queries)
    updateWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(defaultHeader)
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
