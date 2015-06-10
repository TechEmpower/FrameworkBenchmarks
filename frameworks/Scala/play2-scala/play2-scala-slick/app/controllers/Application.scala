package controllers

import java.util.concurrent._
import play.api.Play.current
import play.api.mvc._
import play.api.libs.json.Json
import scala.concurrent._
import models.{Worlds, World, Fortunes, Fortune, WorldsTableQuery, FortunesTableQuery}
import utils._
import utils.DbOperation._
import scala.concurrent.Future
import scala.slick.jdbc.JdbcBackend.Session

import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {

  // Slick code

  private val worldsTable = new WorldsTableQuery
  private val fortunesTable = new FortunesTableQuery

  def getRandomWorlds(n: Int): Future[Seq[World]] = asyncDbOp { implicit session =>
    val random = ThreadLocalRandom.current()
    for (_ <- 1 to n) yield {
      val randomId = random.nextInt(TestDatabaseRows) + 1
      worldsTable.findById(randomId)
    }
  }

  def getFortunes(): Future[Seq[Fortune]] = asyncDbOp { implicit session =>
    fortunesTable.getAll()
  }

  def updateWorlds(n: Int): Future[Seq[World]] = asyncDbOp { implicit session =>
    val random = ThreadLocalRandom.current()
    for (_ <- 1 to n) yield {
      val randomId = random.nextInt(TestDatabaseRows) + 1
      val world = worldsTable.findById(randomId)
      val randomNumber = random.nextInt(10000) + 1
      val updatedWorld = world.copy(randomNumber = randomNumber)
      worldsTable.updateRandom(updatedWorld)
      updatedWorld
    }
  }

  // Common code between Scala database code

  protected val TestDatabaseRows = 10000

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
    getFortunes().map { dbFortunes =>
      val appendedFortunes =  Fortune(0, "Additional fortune added at request time.") :: (dbFortunes.to[List])
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