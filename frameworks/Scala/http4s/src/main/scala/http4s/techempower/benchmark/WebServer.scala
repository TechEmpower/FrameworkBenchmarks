package http4s.techempower.benchmark

import java.util.concurrent.ThreadLocalRandom

import cats.effect._
import cats.instances.list._
import cats.syntax.parallel._
import cats.syntax.traverse._
import io.circe.generic.auto._
import io.circe.syntax._
import doobie._
import doobie.implicits._
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.twirl._

case class Message(message: String)
case class World(id: Int, randomNumber: Int)
case class Fortune(id: Int, message: String)

// Extract queries parameter (with default and min/maxed)
object Queries {
  def unapply(params: Map[String, Seq[String]]): Option[Int] =
    Some(params.getOrElse("queries", Nil).headOption match {
      case None => 1
      case Some(x) =>
        Math.max(1, Math.min(500, scala.util.Try(x.toInt).getOrElse(1)))
    })
}

object WebServer extends IOApp with Http4sDsl[IO] {
  def openDatabase(host: String,
                   poolSize: Int): Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      be <- Blocker[IO] // our blocking EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver",
        s"jdbc:postgresql://$host/hello_world",
        "benchmarkdbuser",
        "benchmarkdbpass",
        ce,
        be
      )
      _ <- Resource.liftF(
        xa.configure(
          ds =>
            IO {
              ds.setMaximumPoolSize(poolSize)
              ds.setMinimumIdle(poolSize)
          }
        )
      )
    } yield xa

  // Provide a random number between 1 and 10000 (inclusive)
  val randomWorldId: IO[Int] = IO(ThreadLocalRandom.current.nextInt(1, 10001))

  // Update the randomNumber field with a random number
  def updateRandomNumber(world: World): IO[World] =
    randomWorldId.map(id => world.copy(randomNumber = id))

  // Select a World object from the database by ID
  def selectWorld(xa: Transactor[IO], id: Int): IO[World] =
    sql"select id, randomNumber from World where id = $id"
      .query[World]
      .unique
      .transact(xa)

  // Select a random World object from the database
  def selectRandomWorld(xa: Transactor[IO]): IO[World] =
    randomWorldId flatMap { id =>
      selectWorld(xa, id)
    }

  // Select a specified number of random World objects from the database
  def getWorlds(xa: Transactor[IO], numQueries: Int): IO[List[World]] =
    (0 until numQueries).toList.parTraverse(_ => selectRandomWorld(xa))

  // Update the randomNumber field with a new random number, for a list of World objects
  def getNewWorlds(worlds: List[World]): IO[List[World]] =
    worlds.traverse(updateRandomNumber)

  // Update the randomNumber column in the database for a specified set of World objects,
  // this uses a batch update SQL call.
  def updateWorlds(xa: Transactor[IO], newWorlds: List[World]): IO[Int] = {
    val sql = "update World set randomNumber = ? where id = ?"
    // Reason for sorting: https://github.com/TechEmpower/FrameworkBenchmarks/pull/4214#issuecomment-489358881
    val update = Update[(Int, Int)](sql)
      .updateMany(newWorlds.sortBy(_.id).map(w => (w.randomNumber, w.id)))
    update.transact(xa)
  }

  // Retrieve all fortunes from the database
  def getFortunes(xa: Transactor[IO]): IO[List[Fortune]] = {
    val query = sql"select id, message from Fortune".query[Fortune]
    query.to[List].transact(xa)
  }

  // Add a new fortune to an existing list, and sort by message.
  def getSortedFortunes(old: List[Fortune]): List[Fortune] = {
    val newFortune = Fortune(0, "Additional fortune added at request time.")
    (newFortune :: old).sortBy(_.message)
  }

  // Add Server header container server address
  def addServerHeader(service: HttpRoutes[IO]): HttpRoutes[IO] =
    cats.data.Kleisli { req: Request[IO] =>
      service.run(req).map(_.putHeaders(Header("Server", req.serverAddr)))
    }

  // HTTP service definition
  def service(xa: Transactor[IO]) =
    addServerHeader(HttpRoutes.of[IO] {
      case GET -> Root / "plaintext" =>
        Ok("Hello, World!")

      case GET -> Root / "json" =>
        Ok(Message("Hello, World!").asJson)

      case GET -> Root / "db" =>
        Ok(selectRandomWorld(xa).map(_.asJson))

      case GET -> Root / "queries" :? Queries(numQueries) =>
        Ok(getWorlds(xa, numQueries).map(_.asJson))

      case GET -> Root / "fortunes" =>
        Ok(for {
          oldFortunes <- getFortunes(xa)
          newFortunes = getSortedFortunes(oldFortunes)
        } yield html.index(newFortunes))

      case GET -> Root / "updates" :? Queries(numQueries) =>
        Ok(for {
          worlds <- getWorlds(xa, numQueries)
          newWorlds <- getNewWorlds(worlds)
          _ <- updateWorlds(xa, newWorlds)
        } yield newWorlds.asJson)
    })

  // Given a fully constructed HttpService, start the server and wait for completion
  def startServer(service: HttpRoutes[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(Router("/" -> service).orNotFound)
      .resource

  // Entry point when starting service
  override def run(args: List[String]): IO[ExitCode] =
    (for {
      db <- openDatabase(
        args.headOption.getOrElse("localhost"),
        sys.env.get("DB_POOL_SIZE").map(_.toInt).getOrElse(256)
      )
      server <- startServer(service(db))
    } yield server)
      .use(_ => IO.never)
      .map(_ => ExitCode.Success)
}
