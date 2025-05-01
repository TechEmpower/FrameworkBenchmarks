import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDateTime, ZoneOffset, ZonedDateTime}
import java.util.Date
import java.util.concurrent.locks.LockSupport
import scala.concurrent.ExecutionContext

object TapirHttp4sServerBenchmark extends IOApp:
  private val STATIC_SERVER_NAME = "tapir-http4s-server"

  private val plainTextMessage: String = "Hello, World!"

  val plaintextEndpoint =
    endpoint.get.in("plaintext")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(stringBody)
      .serverLogic(_ => IO.pure[Either[Unit, String]](Right(plainTextMessage)))

  val jsonEndpoint =
    endpoint.get.in("json")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(jsonBody[Payload])
      .serverLogic(_ => IO.pure[Either[Unit, Payload]](Right(Payload(plainTextMessage))))


  private val declaredPort = 8080
  private val declaredHost = "0.0.0.0"

  val routes = Http4sServerInterpreter[IO]().toRoutes(List(plaintextEndpoint, jsonEndpoint))

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .withExecutionContext(ec)
      .bindHttp(declaredPort, declaredHost)
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource
      .useForever
      .as(ExitCode.Success)
