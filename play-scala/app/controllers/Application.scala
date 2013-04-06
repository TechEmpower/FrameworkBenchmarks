package controllers

import play.api.Play.current
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

    val dbEc = Akka.system.dispatchers.lookup("akka.actor.db")

    Async {
      val random = ThreadLocalRandom.current()

      val worlds = Future.sequence( (for {
            _ <- 1 to queries
          } yield Future(World.findById(random.nextInt(TEST_DATABASE_ROWS) + 1))(dbEc)
        ).toList)

      worlds map {
        w => Ok(Json.toJson(w))  
      } 
    }
  }     
}