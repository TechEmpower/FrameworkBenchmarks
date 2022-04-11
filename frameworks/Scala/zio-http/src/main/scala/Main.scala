import zhttp.service.{EventLoopGroup, Server}
import zio._
import zhttp.http._
import zhttp.service.server.ServerChannelFactory
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import io.netty.util.AsciiString

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

case class Message(message: String)

object Main extends App {
  private val message: String = "Hello, World!"
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make

  private val plaintextPath = "/plaintext"
  private val jsonPath      = "/json"

  private val STATIC_SERVER_NAME = AsciiString.cached("zio-http")

  private val JsonResponse = Response
    .json(writeToString(Message(message)))
    .withServerTime
    .withServer(STATIC_SERVER_NAME)
    .freeze

  private val PlainTextResponse = Response
    .text(message)
    .withServerTime
    .withServer(STATIC_SERVER_NAME)
    .freeze

  private def plainTextApp(response: Response) = Http.fromHExit(HExit.succeed(response)).whenPathEq(plaintextPath)

  private def jsonApp(json: Response) = Http.fromHExit(HExit.succeed(json)).whenPathEq(jsonPath)

  private def app = for {
    plainTextResponse <- PlainTextResponse
    jsonResponse      <- JsonResponse
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




