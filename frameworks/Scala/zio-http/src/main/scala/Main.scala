import zhttp.http._
import zhttp.service.{EventLoopGroup, Server}
import zio.{App, ExitCode, URIO}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import zhttp.http.Response
import zhttp.service.server.ServerChannelFactory

case class Message(message: String)

object Main extends App {
  val message: String                         = "Hello, World!"
  val messageLength: Long = message.size
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make
  val plaintextResp = Response.text(message).addHeader("server", "zio-http")
  val jsonResp = Response.jsonString(writeToString(Message(message))).addHeader("server", "zio-http")

  val app = HttpApp.collect{
    case Method.GET -> !! / "plaintext" => plaintextResp
    case Method.GET -> !! / "json"      => jsonResp

  }
  val server = Server.app(app.silent) ++ Server.port(8080) ++ Server.keepAlive ++ Server.disableLeakDetection ++ Server.memoize ++ Server.serverTime

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = server.make.useForever.provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto()).exitCode

}
