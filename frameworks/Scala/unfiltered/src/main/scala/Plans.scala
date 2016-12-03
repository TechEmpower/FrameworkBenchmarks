package bench

import unfiltered.request._
import unfiltered.response._
import unfiltered.netty._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import org.json4s.JsonDSL.WithBigDecimal._
import scala.util.Random
import slick.driver.MySQLDriver.api._
import slick.jdbc.{ GetResult }
import slick.lifted.{Query => Q}
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

case class World(id: Long, randomNumber: Long)

/** unfiltered plan */
object Plans extends future.Plan
  with cycle.DeferralExecutor with cycle.DeferredIntent with ServerErrorResponse {
  implicit def executionContext = ExecutionContext.Implicits.global

  private val TEST_DATABASE_ROWS = 9999
  implicit val getWorld = GetResult(r => World(r.<<, r.<<))
  implicit val formats = Serialization.formats(NoTypeHints)
  val db = DatabaseAccess.databases("db.default")

  def intent = {
    case GET(Path("/json")) => Future.successful(JsonContent ~> ResponseString(compact(render("message" -> "Hello, World!"))))
    case GET(Path("/db") & Params(params)) =>
      val random = ThreadLocalRandom.current()
      val queries = params.get("queries").flatMap(_.headOption).getOrElse("1").toInt
      db.run(
        DBIO.sequence {
          (0 until queries).map { _ =>
            val next = 1 + random.nextInt(TEST_DATABASE_ROWS)
            sql"select id, randomNumber from World where id = $next".as[World].head
          }
        }
      ).map( result =>
        JsonContent ~> ResponseString(
          write(
            result
          )
        )
      )
  }
  def underlying = CustomExecutor.underlying
}

object CustomExecutor {
  import org.jboss.netty.handler.execution._
  lazy val underlying = new MemoryAwareThreadPoolExecutor(
    DatabaseAccess.maxThreads, 536870912, 536870912
  )
}
