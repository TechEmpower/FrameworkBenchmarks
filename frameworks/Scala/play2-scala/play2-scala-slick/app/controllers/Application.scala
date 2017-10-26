package controllers

import java.util.concurrent._
import javax.inject.{Singleton, Inject}
import play.api.mvc._
import play.mvc.Http
import play.api.libs.json.Json
import models._
import scala.concurrent.{Future, ExecutionContext}

@Singleton()
class Application @Inject() (fortunesDAO: FortunesDAO, worldDAO: WorldDAO, val controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
  extends BaseController {

  val defaultHeader = Http.HeaderNames.SERVER -> "Play Framework"

  def getRandomWorlds(n: Int): Future[Seq[World]] = {
    val worlds: Seq[Future[World]] = for (_ <- 1 to n) yield {
      worldDAO.findById(getNextRandom)
    }
    Future.sequence(worlds)
  }

  def getFortunes: Future[Seq[Fortune]] = {
    fortunesDAO.getAll()
  }

  def updateWorlds(n: Int): Future[Seq[World]] = {
    val worlds: Seq[Future[World]] = for (_ <- 1 to n) yield {
      val futureWorld: Future[World] = worldDAO.findById(getNextRandom)
      val futureUpdatedWorld: Future[World] = futureWorld.map(_.copy(randomNumber = getNextRandom))
      futureUpdatedWorld.map(world => worldDAO.updateRandom(world))
      futureUpdatedWorld
    }
    Future.sequence(worlds)
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

  // Common code between Scala database code

  protected val TestDatabaseRows = 10000

  import models.WorldJsonHelpers.toJson

  def db = Action.async {
    getRandomWorlds(1).map { worlds =>
      Ok(Json.toJson(worlds.head)).withHeaders(defaultHeader)
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(defaultHeader)
    }
  }

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes =  Fortune(0, "Additional fortune added at request time.") :: dbFortunes.to[List]
      Ok(views.html.fortune(appendedFortunes)).withHeaders(defaultHeader).as(HTML)
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
