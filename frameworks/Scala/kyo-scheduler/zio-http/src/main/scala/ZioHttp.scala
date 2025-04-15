import zio._
import zio.http._
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.json.*

import java.lang.{Runtime => JRuntime}

// based on the framework/Scala/zio-http implementation
object ZioHttp extends kyo.KyoSchedulerZIOAppDefault {

  private val plainTextMessage: String = "hello, world!"

  private val STATIC_SERVER_NAME = "zio-http"
  private val NUM_PROCESSORS     = JRuntime.getRuntime.availableProcessors()

  val app: Routes[Any, Response] = Routes(
    Method.GET / "/plaintext" ->
      Handler.fromResponse(
        Response
          .text(plainTextMessage)
          .addHeader(Header.Server(STATIC_SERVER_NAME)),
      ),
    Method.GET / "/json"      ->
      Handler.fromResponse(
        Response
          .json(Payload(plainTextMessage).toJson)
          .addHeader(Header.Server(STATIC_SERVER_NAME)),
      ),
  )

  private val config = Server.Config.default
    .port(8080)
    .enableRequestStreaming

  private val nettyConfig = NettyConfig.default
    .leakDetection(LeakDetectionLevel.DISABLED)
    .maxThreads(NUM_PROCESSORS)

  private val configLayer      = ZLayer.succeed(config)
  private val nettyConfigLayer = ZLayer.succeed(nettyConfig)

  val run: UIO[ExitCode] =
    Server.serve(app).provide(configLayer, nettyConfigLayer, Server.customized).exitCode
}