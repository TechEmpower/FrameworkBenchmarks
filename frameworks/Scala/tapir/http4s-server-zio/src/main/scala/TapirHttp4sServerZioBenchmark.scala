import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import sttp.model.{Header, HeaderNames, StatusCode}
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.*
import zio.*

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDateTime, ZoneOffset, ZonedDateTime}
import java.util.Date
import java.util.concurrent.locks.LockSupport
import scala.concurrent.ExecutionContext
import zio.interop.catz.*

object TapirHttp4sServerZioBenchmark extends ZIOAppDefault:
  private val STATIC_SERVER_NAME = "tapir-http4s-server-zio"

  private val plainTextMessage: String = "Hello, World!"

  private val plaintextEndpoint: ZServerEndpoint[Any, Any] =
    endpoint.get.in("plaintext")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(stringBody)
      .zServerLogic(_ => ZIO.succeed(plainTextMessage))

  private val jsonEndpoint: ZServerEndpoint[Any, Any] =
    endpoint.get.in("json")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(jsonBody[Payload])
      .zServerLogic(_ => ZIO.succeed(Payload(plainTextMessage)))


  private val declaredPort = 8080
  private val declaredHost = "0.0.0.0"

  private val routes: HttpRoutes[Task] = ZHttp4sServerInterpreter()
    .from(List(plaintextEndpoint, jsonEndpoint))
    .toRoutes

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  override def run: URIO[Any, ExitCode] =
    ZIO.executor.flatMap(executor =>
      BlazeServerBuilder[Task]
        .withExecutionContext(executor.asExecutionContext)
        .bindHttp(8080, declaredHost)
        .withHttpApp(Router("/" -> routes).orNotFound)
        .serve
        .compile
        .drain
    ).exitCode
