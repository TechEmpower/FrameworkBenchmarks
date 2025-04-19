import sttp.model.{Header, HeaderNames}
import sttp.tapir.Schema
import sttp.tapir.json.zio.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.ztapir.*
import zio.*
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.http.{Request, Response, Routes, Server}
import zio.json.*

import java.lang.Runtime as JRuntime

object TapirZioHttpServerBenchmark extends ZIOAppDefault {
  private val STATIC_SERVER_NAME = "zio-http-tapir"

  private val plainTextMessage: String = "Hello, World!"
  private val NUM_PROCESSORS     = JRuntime.getRuntime.availableProcessors()

  given JsonCodec[Payload] = DeriveJsonCodec.gen
  given Schema[Payload] = Schema.derived

  override def run = {
    val plaintextRoute: ZServerEndpoint[Any, Any] =
        endpoint.get.in("plaintext")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(stringBody)
          .zServerLogic { _ =>
            ZIO.succeed(plainTextMessage)
          }

    val jsonRoute: ZServerEndpoint[Any, Any] =
        endpoint.get.in("json")
          .out(header(HeaderNames.Server, STATIC_SERVER_NAME))
          .out(jsonBody[Payload])
          .zServerLogic { _ =>
            ZIO.succeed(Payload(plainTextMessage))
          }

    val app = ZioHttpInterpreter().toHttp(List(plaintextRoute, jsonRoute))


    val config = Server.Config.default
      .port(8080)
      .enableRequestStreaming

    val nettyConfig = NettyConfig.default
      .leakDetection(LeakDetectionLevel.DISABLED)
      .maxThreads(NUM_PROCESSORS)

    val configLayer = ZLayer.succeed(config)
    val nettyConfigLayer = ZLayer.succeed(nettyConfig)

    Server.serve(app).provide(configLayer, nettyConfigLayer, Server.customized).exitCode
  }
}