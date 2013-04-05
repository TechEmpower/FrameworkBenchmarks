package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.concurrent._
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent._
import models._

object Application extends Controller {
  
  private val TEST_DATABASE_ROWS = 10000

  def json() = Action {
    Ok(Json.obj("message" -> "Hello World!"))   
  }

  def db(queries: Int) = Action {
    import play.api.libs.concurrent.Execution.Implicits._

    Async {
      val random = ThreadLocalRandom.current()

      val worlds = Future.sequence( (for {
        _ <- (1 to queries).par
      } yield Future(World.findById(random.nextInt(TEST_DATABASE_ROWS) + 1))).toList)

      worlds map {
        w => Ok(Json.toJson(w))  
      } 
    }
  }     
}