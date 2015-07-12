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

  def randomWorldId = ThreadLocalRandom.current.nextInt(1, 10001)

  def selectRandomWorld(xa: Transactor[Task]): Task[World] = {
    val query = sql"select id, randomNumber from World where id = $randomWorldId".query[World]
    query.unique.transact(xa)
  }

  def getWorlds(xa: Transactor[Task], numQueries: Int): Task[List[World]] =
    Nondeterminism[Task].replicateM(numQueries, selectRandomWorld(xa))

  def updateWorlds(xa: Transactor[Task], worlds: List[World]): Task[List[World]] = {
    val newWorlds = worlds.map(_.copy(randomNumber = randomWorldId))
    val sql = "update World set randomNumber = ? where id = ?"
    val update = Update[(Int, Int)](sql).updateMany(newWorlds.map(w => (w.randomNumber, w.id)))
    update.transact(xa).map(_ => newWorlds)
  }

  def service(xa: Transactor[Task]) = HttpService {
    case GET -> Root / "json" =>
      Ok(Json("message" -> jString("Hello, World!")))

    case GET -> Root / "db" =>
      Ok(selectRandomWorld(xa).map(_.asJson))

    case GET -> Root / "queries" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)
      Ok(getWorlds(xa, numQueries).map(_.asJson))

    case GET -> Root / "updates" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)

      val updated = for {
        worlds <- getWorlds(xa, numQueries)
        updatedWorlds <- updateWorlds(xa, worlds)
      } yield updatedWorlds

      Ok(updated.map(_.asJson))

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
