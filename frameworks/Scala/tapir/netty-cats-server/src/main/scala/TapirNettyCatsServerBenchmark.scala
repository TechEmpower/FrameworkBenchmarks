import cats.effect.{IO, IOApp}
import io.circe.generic.auto.*
import sttp.model.{Header, HeaderNames}
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.netty.cats.NettyCatsServer

import java.time.Instant

object TapirNettyCatsServerBenchmark extends IOApp.Simple:
  private val STATIC_SERVER_NAME = "tapir-netty-cats-server"

  private val plainTextMessage: String = "Hello, World!"

  val plaintextEndpoint =
    endpoint.get.in("plaintext")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(header[String](HeaderNames.Date))
      .out(stringBody)
      .serverLogic(_ =>
        for {
          now <- IO.realTime.map(time => Instant.ofEpochMilli(time.toMillis))
        } yield Right(Header.toHttpDateString(now) -> plainTextMessage)
      )

  val jsonEndpoint =
    endpoint.get.in("json")
      .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
      .out(header[String](HeaderNames.Date))
      .out(jsonBody[Payload])
      .serverLogic(_ =>
        for {
          now <- IO.realTime.map(time => Instant.ofEpochMilli(time.toMillis))
        } yield Right(Header.toHttpDateString(now) -> Payload(plainTextMessage))
      )


  private val declaredPort = 8080
  private val declaredHost = "0.0.0.0"

  override def run = NettyCatsServer
    .io()
    .use { server =>
      for {
        _ <- server
          .port(declaredPort)
          .host(declaredHost)
          .addEndpoint(plaintextEndpoint)
          .addEndpoint(jsonEndpoint)
          .start()
        _ <- IO.never
      } yield ()
    }
