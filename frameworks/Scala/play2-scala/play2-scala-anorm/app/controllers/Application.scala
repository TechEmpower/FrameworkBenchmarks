package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc._
import play.api.libs.json.Json
import java.util.concurrent._
import models.{WorldDAO, FortunesDAO, World, Fortune}
import utils.DbOperation

@Singleton()
class Application @Inject() (fortunesDAO: FortunesDAO, worldDAO: WorldDAO, dbOperation: DbOperation, val controllerComponents: ControllerComponents)
  extends BaseController {

  def getRandomWorlds(n: Int): Seq[World] = dbOperation.syncDbOp { implicit connection =>
    for (_ <- 1 to n) yield {
      worldDAO.findById(getNextRandom)
    }
  }

  def getFortunes: Seq[Fortune] = dbOperation.syncDbOp { implicit connection =>
    fortunesDAO.getAll
  }

  def updateWorlds(n: Int): Seq[World] = dbOperation.syncDbOp { implicit connection =>
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

  def db = Action {
    Ok(Json.toJson(getRandomWorlds(1).head))
  }

  def queries(countString: String) = Action {
    val n = parseCount(countString)
    Ok(Json.toJson(getRandomWorlds(n)))
  }

  def fortunes() = Action {
    val appendedFortunes =  Fortune(0, "Additional fortune added at request time.") :: getFortunes.to[List]
    Ok(views.html.fortune(appendedFortunes))
  }

  def update(queries: String) = Action {
    val n = parseCount(queries)
    Ok(Json.toJson(updateWorlds(n)))
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
