package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc._
import play.api.libs.json.Json
import java.util.concurrent._
import models.{WorldDAO, FortunesDAO, World, Fortune}
import utils.DbOperation
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits._

@Singleton()
class Application @Inject() (fortunesDAO: FortunesDAO, worldDAO: WorldDAO, dbOperation: DbOperation)  extends Controller {

  // Anorm code

  def getRandomWorlds(n: Int): Future[Seq[World]] = dbOperation.asyncDbOp { implicit connection =>
    for (_ <- 1 to n) yield {
      worldDAO.findById(getNextRandom)
    }
  }

  def getFortunes: Future[Seq[Fortune]] = dbOperation.asyncDbOp { implicit connection =>
    fortunesDAO.getAll
  }

  def updateWorlds(n: Int): Future[Seq[World]] = dbOperation.asyncDbOp { implicit connection =>
    for(_ <- 1 to n) yield {
      val world = worldDAO.findById(getNextRandom)
      val updatedWorld = world.copy(randomNumber = getNextRandom)
      worldDAO.updateRandom(updatedWorld)
      updatedWorld
    }
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

  // Common code between Scala database code

  protected val TestDatabaseRows = 10000

  import models.WorldJsonHelpers.toJson

  def db = Action.async {
    getRandomWorlds(1).map { worlds =>
      Ok(Json.toJson(worlds.head)).withHeaders(CONTENT_TYPE -> "application/json")
    }
  }

  def queries(countString: String) = Action.async {
    val n = parseCount(countString)
    getRandomWorlds(n).map { worlds =>
      Ok(Json.toJson(worlds)).withHeaders(CONTENT_TYPE -> "application/json")
    }
  }

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes =  Fortune(0, "Additional fortune added at request time.") :: dbFortunes.to[List]
      Ok(views.html.fortune(appendedFortunes)).withHeaders(CONTENT_TYPE -> "text/html")
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