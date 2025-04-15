import sttp.model.{Header, HeaderNames}
import sttp.tapir.Schema
import sttp.tapir.json.zio.*
import sttp.tapir.server.netty.zio.NettyZioServer
import sttp.tapir.ztapir.*
import zio.*
import zio.json.*
import sttp.tapir.server.netty.*

object TapirNettyZioServerBenchmark extends ZIOAppDefault {
  private val STATIC_SERVER_NAME = "tapir-netty-zio-server"

  private val plainTextMessage: String = "Hello, World!"

  given JsonCodec[Payload] = DeriveJsonCodec.gen
  given Schema[Payload] = Schema.derived

  override def run = {
    val plaintextRoute: ZServerEndpoint[Any, Any] =
        endpoint.get.in("plaintext")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(header[String](HeaderNames.Date))
          .out(stringBody)
          .zServerLogic { _ =>
            for {
              now <- Clock.currentDateTime
            } yield Header.toHttpDateString(now.toInstant) -> plainTextMessage
          }

    val jsonRoute: ZServerEndpoint[Any, Any] =
        endpoint.get.in("json")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(header[String](HeaderNames.Date))
          .out(jsonBody[Payload])
          .zServerLogic { _ =>
            for {
              now <- Clock.currentDateTime
            } yield Header.toHttpDateString(now.toInstant) -> Payload(plainTextMessage)
          }

    val config = NettyConfig.default
      .withSocketKeepAlive
      .copy(lingerTimeout = None)


    
    val server = NettyZioServer[Any](config)
      .addEndpoint(plaintextRoute)
      .addEndpoint(jsonRoute)
      .host("0.0.0.0")
      .port(8080)

    server.start().flatMap { _ =>
      ZIO.never
    }
  }
}