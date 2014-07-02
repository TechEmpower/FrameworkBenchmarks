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

    private val TestDatabaseRows = 10000

    def db(queries: Int) =
        Action {
            transactional {
                val random = ThreadLocalRandom.current()

                val worlds =
                    for (_ <- 1 to queries) yield {
                        ActivateWorld.fingByLegacyId(random.nextInt(TestDatabaseRows) + 1)
                    }

                Ok(Json.toJson(worlds))
            }
        }

    def fortunes() =
        Action {
            val transaction = new Transaction
            try
                transactional(transaction) {
                    val fortunes =
                        ActivateFortune.all ++ List(new ActivateFortune(0, "Additional fortune added at request time."))
                    Ok(views.html.fortune(fortunes))
                }
            finally
                transaction.rollback
        }

    def update(queries: Int) =
        Action {

            val random = ThreadLocalRandom.current()

            transactional {
                val worlds =
                    for (_ <- 1 to queries) yield {
                        val world = ActivateWorld.fingByLegacyId(random.nextInt(TestDatabaseRows) + 1)
                        world.randomNumber = random.nextInt(TestDatabaseRows) + 1
                        world
                    }
                Ok(Json.toJson(worlds)).withHeaders("Server" -> "Netty")
            }
        }
}