import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import zhttp.http._
import zio._
import zhttp.service._
import zhttp.service.server.ServerChannelFactory
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.netty.util.AsciiString

object Main extends App {
  private val plainTextMessage: String = "Hello, World!"
  private val jsonMessage: String      = """{"message":"Hello, World!"}"""

  private val plaintextPath = "/plaintext"
  private val jsonPath      = "/json"

  private val STATIC_SERVER_NAME = AsciiString.cached("zio-http")

  private val frozenJsonResponse = Response
    .json(jsonMessage)
    .withServerTime
    .withServer(STATIC_SERVER_NAME)
    .freeze

  private val frozenPlainTextResponse = Response
    .text(plainTextMessage)
    .withServerTime
    .withServer(STATIC_SERVER_NAME)
    .freeze

  private def plainTextApp(response: Response) = Http.fromHExit(HExit.succeed(response)).whenPathEq(plaintextPath)

  private def jsonApp(json: Response) = Http.fromHExit(HExit.succeed(json)).whenPathEq(jsonPath)

  private def app = for {
    plainTextResponse <- frozenPlainTextResponse
    jsonResponse      <- frozenJsonResponse
  } yield plainTextApp(plainTextResponse) ++ jsonApp(jsonResponse)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    app
      .flatMap(server(_).make.useForever)
      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(8))
      .exitCode

  private def server(app: HttpApp[Any, Nothing]) =
    Server.app(app) ++
      Server.port(8080) ++
      Server.error(_ => UIO.unit) ++
      Server.disableLeakDetection ++
      Server.consolidateFlush ++
      Server.disableFlowControl
}




