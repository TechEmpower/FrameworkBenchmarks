package controllers

import play.api.Play.current
import play.api.mvc._
import play.api.libs.json.Json
import java.util.concurrent._
import scala.concurrent._
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import play.core.NamedThreadFactory
import models.ActivateWorld
import models.Models._
import models.ActivateFortune
import models.persistenceContext._

object Application extends Controller {

  def getRandomWorlds(n: Int): Seq[ActivateWorld] = {
      val random = ThreadLocalRandom.current()
      for (_ <- 1 to n) yield {
          val randomId = random.nextInt(TestDatabaseRows) + 1
          ActivateWorld.fingByLegacyId(randomId)
      }
  }

  def getFortunes(): Seq[ActivateFortune] = {
      ActivateFortune.all
  }

  def updateWorlds(n: Int): Seq[ActivateWorld] = {
      val random = ThreadLocalRandom.current()
      for (_ <- 1 to n) yield {
            val randomId = random.nextInt(TestDatabaseRows) + 1
            val world = ActivateWorld.fingByLegacyId(randomId)
            world.randomNumber = random.nextInt(10000) + 1
            world
      }
  }

  // Common(ish) code between Scala database code

  protected val TestDatabaseRows = 10000

  def db = Action {
      transactional {
          val worlds = getRandomWorlds(1)
          Ok(Json.toJson(worlds.head))
      }
  }

  def queries(countString: String) = Action {
      val n = parseCount(countString)
      transactional {
          val worlds = getRandomWorlds(n)
          Ok(Json.toJson(worlds))
      }
  }

  def fortunes() = Action {
      val transaction = new Transaction
      try
          transactional(transaction) {
              val dbFortunes = getFortunes()
              val appendedFortunes =  new ActivateFortune(0, "Additional fortune added at request time.") :: (dbFortunes.to[List])
              Ok(views.html.fortune(appendedFortunes))
          }
      finally
          transaction.rollback
  }

  def update(queries: String) = Action {
      transactional {
          val n = parseCount(queries)
          val worlds = updateWorlds(n)
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
