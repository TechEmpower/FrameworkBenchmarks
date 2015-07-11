import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder
import headers._

import _root_.argonaut._, Argonaut._, Shapeless._

import doobie.contrib.hikari.hikaritransactor._
import doobie.imports._

import scalaz.concurrent.Task

import java.util.concurrent.ThreadLocalRandom

case class World(id: Int, randomNumber: Int)

object Middleware {
  def addHeaders(service: HttpService): HttpService = {
    Service.lift { req: Request =>
      service.map { resp =>
        resp.putHeaders(
          headers.Date(DateTime.now),
          Header("Server", req.serverName)
        )
      }(req)
    }
  }
}


object WebServer extends App {
  val xaTask = HikariTransactor[Task]("org.postgresql.Driver", "jdbc:postgresql:hello_world", "benchmarkdbuser", "benchmarkdbpass")

  def service(xa: HikariTransactor[Task]) = HttpService {
    case GET -> Root / "json" =>
      Ok(Json("message" -> jString("Hello, World!")))

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!")
        .withContentType(Some(`Content-Type`(MediaType.`text/plain`)))

    case GET -> Root / "db" =>
      val rnd = ThreadLocalRandom.current.nextInt(1, 10001)
      val query = sql"select id, randomNumber from World where id = $rnd".query[World].unique
      Ok(query.transact(xa).map(_.asJson))
  }

  xaTask.map { xa =>
    BlazeBuilder.bindHttp(8080, "0.0.0.0")
      .mountService(Middleware.addHeaders(service(xa)), "/")
      .run
      .awaitShutdown()
  }.run
}
