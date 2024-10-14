import zio._
import zio.http._
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel

object Main extends ZIOAppDefault {

  private val plainTextMessage: String = "hello, world!"
  private val jsonMessage: String      = """{"message": "hello, world!"}"""

  private val STATIC_SERVER_NAME = "zio-http"
  private val NUM_PROCESSORS     = Runtime.getRuntime.availableProcessors()

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
          .json(jsonMessage)
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
    Server.serve(app.toHttpApp).provide(configLayer, nettyConfigLayer, Server.customized).exitCode
}