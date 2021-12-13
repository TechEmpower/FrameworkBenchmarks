package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc._
import play.mvc.Http
import play.api.libs.json.Json
import java.util.concurrent._
import models.{WorldDAO, FortunesDAO, World, Fortune}
import utils.DbOperation
import scala.concurrent.{Future, ExecutionContext}

@Singleton()
class Application @Inject() (fortunesDAO: FortunesDAO, worldDAO: WorldDAO, dbOperation: DbOperation, val controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
  extends BaseController {

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


  // Common code between Scala database code

  protected val TestDatabaseRows = 10000

  import models.WorldJsonHelpers.toJson

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

  def fortunes() = Action.async {
    getFortunes.map { dbFortunes =>
      val appendedFortunes = List(Fortune(0, "Additional fortune added at request time.")) ++ dbFortunes
      Ok(views.html.fortune(appendedFortunes))
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
