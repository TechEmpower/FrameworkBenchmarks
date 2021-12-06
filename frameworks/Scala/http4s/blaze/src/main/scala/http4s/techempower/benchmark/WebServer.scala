package http4s.techempower.benchmark

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.typesafe.config.ConfigValueFactory
import io.circe.generic.auto._
import io.circe.syntax._
import io.getquill.util.LoadConfig
import io.getquill.LowerCase
import io.getquill.PostgresJAsyncContext
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.headers.Server
import org.http4s.twirl._

final case class Message(message: String)
final case class World(id: Int, randomNumber: Int)
final case class Fortune(id: Int, message: String)

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
  def makeDatabaseService(
      host: String,
      poolSize: Int
  ): Resource[IO, DatabaseService] = {
    for {
      executor <- Resource(IO {
        val pool = Executors.newFixedThreadPool(poolSize)
        (pool, IO(pool.shutdown()))
      })
      ctx <- Resource.fromAutoCloseable(IO(new PostgresJAsyncContext(
        LowerCase,
        LoadConfig("ctx")
          .withValue("host", ConfigValueFactory.fromAnyRef(host))
          .withValue(
            "maxActiveConnections",
            ConfigValueFactory.fromAnyRef(poolSize)
          )
      )))
    } yield new DatabaseService(ctx, executor)
  }

  // Add a new fortune to an existing list, and sort by message.
  def getSortedFortunes(old: List[Fortune]): List[Fortune] = {
    val newFortune = Fortune(0, "Additional fortune added at request time.")
    (newFortune :: old).sortBy(_.message)
  }

  // Add Server header container server address
  def addServerHeader(service: HttpRoutes[IO]): HttpRoutes[IO] =
    cats.data.Kleisli { req: Request[IO] =>
      service.run(req).map(_.putHeaders(server))
    }

  val server = Server(ProductId("http4s", None))

  // HTTP service definition
  def service(db: DatabaseService) =
    addServerHeader(HttpRoutes.of[IO] {
      case GET -> Root / "plaintext" =>
        Ok("Hello, World!")

      case GET -> Root / "json" =>
        Ok(Message("Hello, World!").asJson)

      case GET -> Root / "db" =>
        Ok(db.selectRandomWorld().map(_.asJson))

      case GET -> Root / "queries" :? Queries(numQueries) =>
        Ok(db.getWorlds(numQueries).map(_.asJson))

      case GET -> Root / "fortunes" =>
        Ok(for {
          oldFortunes <- db.getFortunes()
          newFortunes = getSortedFortunes(oldFortunes)
        } yield html.index(newFortunes))

      case GET -> Root / "updates" :? Queries(numQueries) =>
        Ok(for {
          worlds <- db.getWorlds(numQueries)
          newWorlds <- db.getNewWorlds(worlds)
          _ <- db.updateWorlds(newWorlds)
        } yield newWorlds.asJson)
    })

  val blazeEc = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(32))

  // Given a fully constructed HttpService, start the server and wait for completion
  def startServer(service: HttpRoutes[IO]) =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(Router("/" -> service).orNotFound)
      .withSocketKeepAlive(true)
      .resource

  // Entry point when starting service
  override def run(args: List[String]): IO[ExitCode] =
    (for {
      db <- makeDatabaseService(
        args.headOption.getOrElse("localhost"),
        sys.env.get("DB_POOL_SIZE").map(_.toInt).getOrElse(64)
      )
      server <- startServer(service(db))
    } yield server)
      .use(_ => IO.never)
}
