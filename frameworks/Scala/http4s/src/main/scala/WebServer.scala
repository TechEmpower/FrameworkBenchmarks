import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder
import headers._

import _root_.argonaut._, Argonaut._, Shapeless._

import doobie.contrib.hikari.hikaritransactor._
import doobie.imports._

import scalaz._
import scalaz.concurrent.{Task, TaskApp}

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

object Queries extends OptionalValidatingQueryParamDecoderMatcher[Int]("queries") {
  def clampQueries(numQueries: Option[ValidationNel[ParseFailure, Int]]): Int = {
    numQueries.fold(1)(_.fold(
      errors => 1,
      queries => {
        if (queries < 1)
          1
        else if (queries > 500)
          500
        else
          queries
      }
    ))
  }
}

object WebServer extends TaskApp {
  def xaTask(host: String) =
    HikariTransactor[Task]("org.postgresql.Driver", s"jdbc:postgresql://$host/hello_world", "benchmarkdbuser", "benchmarkdbpass")

  def selectRandomWorld(xa: Transactor[Task]): Task[World] = {
    val rnd = ThreadLocalRandom.current.nextInt(1, 10001)
    val query = sql"select id, randomNumber from World where id = $rnd".query[World]
    query.unique.transact(xa)
  }

  def service(xa: Transactor[Task]) = HttpService {
    case GET -> Root / "json" =>
      Ok(Json("message" -> jString("Hello, World!")))

    case GET -> Root / "db" =>
      Ok(selectRandomWorld(xa).map(_.asJson))

    case GET -> Root / "queries" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)
      val parallelQueries = Nondeterminism[Task].replicateM(numQueries, selectRandomWorld(xa))
      Ok(parallelQueries.map(_.asJson))

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!")
        .withContentType(Some(`Content-Type`(MediaType.`text/plain`)))
  }

  def startServer(service: HttpService): Task[Unit] = {
    Task.delay {
      BlazeBuilder.bindHttp(8080, "0.0.0.0")
        .mountService(service, "/")
        .run
        .awaitShutdown()
    }
  }

  override val runc = {
    for {
      dbHost <- Task.delay(System.getProperty("DBHOST", "localhost"))
      xa <- xaTask(dbHost)
      server <- startServer(Middleware.addHeaders(service(xa)))
    } yield ()
  }
}
