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
import scala.slick.driver.MySQLDriver.simple._
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import java.util.concurrent.ThreadLocalRandom

case class World(id: Long, randomNumber: Long)

/** unfiltered plan */
object Plans extends cycle.Plan
  with cycle.DeferralExecutor with cycle.DeferredIntent with ServerErrorResponse {

  private val TEST_DATABASE_ROWS = 9999
  implicit val getWorld = GetResult(r => World(r.<<, r.<<))
  implicit val formats = Serialization.formats(NoTypeHints)
  val db = DatabaseAccess.databases("db.default")

  def intent = {
    case GET(Path("/json")) => JsonContent ~> ResponseString(compact(render("message" -> "Hello world")))
    case GET(Path("/db") & Params(params)) =>
      val random = ThreadLocalRandom.current()
      val queries = params.get("queries").flatMap(_.headOption).getOrElse("1").toInt
      JsonContent ~> ResponseString(
        write(
          db.withSession { implicit session: Session =>
            (0 until queries).map { _ =>
              (Q[Int, World] + "select id, randomNumber from World where id = ?")(random.nextInt(TEST_DATABASE_ROWS) + 1).first
            }
          }
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
