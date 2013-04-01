package controllers

import play._
import play.api.libs.concurrent._
import play.api.mvc._
import play.libs.Json
import org.codehaus.jackson.node.ObjectNode
import views.html._
import models._
import java.util._
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent._

object Application extends Controller {
  private val TEST_DATABASE_ROWS = 10000

  def json() = Action {
    val result = Json.newObject()
    result.put("message", "Hello World!")
    Ok(result.toString)
  }

  def db(queries: Int) = Action {
    import play.api.libs.concurrent.Execution.Implicits._

    val random = ThreadLocalRandom.current()

    Async {
      Future {
        (1 to queries) map {
          _ =>
            World.find.byId(random.nextInt(TEST_DATABASE_ROWS) + 1)
        }
      } map {
        worlds =>
          Ok(Json.toJson(worlds).toString())
      }
    }
  }
}
