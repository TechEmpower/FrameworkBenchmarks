package http4s.techempower.benchmark

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.server._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.twirl._
import headers._

import doobie.contrib.hikari.hikaritransactor._
import doobie.imports._

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import scalaz._
import scalaz.concurrent.{Task, TaskApp}
import scalaz.std.list._
import scalaz.syntax.traverse._

import java.util.concurrent.ThreadLocalRandom

case class World(id: Int, randomNumber: Int)

case class Fortune(id: Int, message: String)

object Middleware {
  def addHeaders(service: HttpService): HttpService = {
    Service.lift { req: Request =>
      service.map { resp =>
        resp.putHeaders(
          Header("Server", req.serverAddr)
        )
      }.apply(req)
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
  implicit def jsonEncoder[A](implicit encoder: Encoder[A]) = jsonEncoderOf[A](encoder)

  def xaTask(host: String) = {
    val driver = "org.postgresql.Driver"
    val url = s"jdbc:postgresql://$host/hello_world"
    val user = "benchmarkdbuser"
    val pass = "benchmarkdbpass"
    val maxPoolSize = 256
    val minIdle = 256

    for {
      xa <- HikariTransactor[Task](driver, url, user, pass)
      _  <- xa.configure(ds => Task.delay {
         ds.setMaximumPoolSize(maxPoolSize)
         ds.setMinimumIdle(minIdle)
      })
    } yield xa
  }

  // Provide a random number between 1 and 10000 (inclusive)
  val randomWorldId: Task[Int] = Task.delay(ThreadLocalRandom.current.nextInt(1, 10001))

  // Update the randomNumber field with a random number
  def updateRandomNumber(world: World): Task[World] = {
    randomWorldId map { id =>
      world.copy(randomNumber = id)
    }
  }

  // Select a World object from the database by ID
  def selectWorld(xa: Transactor[Task], id: Int): Task[World] = {
    val query = sql"select id, randomNumber from World where id = $id".query[World]
    query.unique.transact(xa)
  }

  // Select a random World object from the database
  def selectRandomWorld(xa: Transactor[Task]): Task[World] = {
    randomWorldId flatMap { id =>
      selectWorld(xa, id)
    }
  }

  // Select a specified number of random World objects from the database
  def getWorlds(xa: Transactor[Task], numQueries: Int): Task[List[World]] =
    Nondeterminism[Task].replicateM(numQueries, selectRandomWorld(xa))

  // Update the randomNumber field with a new random number, for a list of World objects
  def getNewWorlds(worlds: List[World]): Task[List[World]] = {
    worlds.traverse(updateRandomNumber)
  }

  // Update the randomNumber column in the database for a specified set of World objects,
  // this uses a batch update SQL call.
  def updateWorlds(xa: Transactor[Task], newWorlds: List[World]): Task[Int] = {
    val sql = "update World set randomNumber = ? where id = ?"
    val update = Update[(Int, Int)](sql).updateMany(newWorlds.map(w => (w.randomNumber, w.id)))
    update.transact(xa)
  }

  // Retrieve all fortunes from the database
  def getFortunes(xa: Transactor[Task]): Task[List[Fortune]] = {
    val query = sql"select id, message from Fortune".query[Fortune]
    query.list.transact(xa)
  }

  // Add a new fortune to an existing list, and sort by message.
  def getSortedFortunes(old: List[Fortune]): List[Fortune] = {
    val newFortune = Fortune(0, "Additional fortune added at request time.")
    (newFortune :: old).sortBy(_.message)
  }

  // HTTP service definition
  def service(xa: Transactor[Task]) = HttpService {
    case GET -> Root / "json" =>
      Ok(Json.obj("message" -> Json.string("Hello, World!")))

    case GET -> Root / "db" =>
      Ok(selectRandomWorld(xa))

    case GET -> Root / "queries" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)
      Ok(getWorlds(xa, numQueries))

    case GET -> Root / "fortunes" =>
      val page = for {
        oldFortunes <- getFortunes(xa)
        newFortunes = getSortedFortunes(oldFortunes)
      } yield html.index(newFortunes)

      Ok(page)

    case GET -> Root / "updates" :? Queries(rawQueries) =>
      val numQueries = Queries.clampQueries(rawQueries)

      val updated = for {
        worlds <- getWorlds(xa, numQueries)
        newWorlds <- getNewWorlds(worlds)
        _ <- updateWorlds(xa, newWorlds)
      } yield newWorlds

      Ok(updated)

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!")
        .withContentType(Some(`Content-Type`(MediaType.`text/plain`)))
  }

  // Given a fully constructed HttpService, start the server and wait for completion
  def startServer(service: HttpService): Task[Unit] = {
    Task.delay {
      BlazeBuilder.bindHttp(8080, "0.0.0.0")
        .mountService(service, "/")
        .run
        .awaitShutdown()
    }
  }

  // Entry point when starting service
  override def runl(args: List[String]): Task[Unit] = {
    for {
      xa <- xaTask(args.head)
      server <- startServer(Middleware.addHeaders(service(xa)))
    } yield ()
  }
}
