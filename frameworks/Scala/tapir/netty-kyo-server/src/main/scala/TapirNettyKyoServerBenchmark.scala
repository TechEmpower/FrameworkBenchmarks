import kyo.*
import sttp.model.{Header, HeaderNames}
import sttp.tapir.*
import sttp.tapir.json.zio.*
import sttp.tapir.server.netty.*
import zio.json.*

object TapirNettyKyoServerBenchmark extends KyoApp {
  private val STATIC_SERVER_NAME = "kyo-tapir"

  private val plainTextMessage: String = "Hello, World!"

  private given JsonCodec[Payload] = DeriveJsonCodec.gen
  private given Schema[Payload] = Schema.derived

  run {
    val plaintextRoute: Unit < Routes =
      Routes.add(
        _.get.in("plaintext")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(header[String](HeaderNames.Date))
          .out(stringBody)
      ) { _ =>
        for {
          now <- Clock.now
        } yield Header.toHttpDateString(now.toJava) -> plainTextMessage
      }

    val jsonRoute: Unit < Routes =
      Routes.add(
        _.get.in("json")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(header[String](HeaderNames.Date))
          .out(jsonBody[Payload])
      ) { _ =>
        for {
          now <- Clock.now
        } yield Header.toHttpDateString(now.toJava) -> Payload(plainTextMessage)
      }

    val config = NettyConfig.default
      .withSocketKeepAlive
      .copy(lingerTimeout = None)
    
    val server = NettyKyoServer(config).host("0.0.0.0").port(8080)

    val binding: NettyKyoServerBinding < Async =
      Routes.run(server)(plaintextRoute.andThen(jsonRoute))

    binding
  }
}