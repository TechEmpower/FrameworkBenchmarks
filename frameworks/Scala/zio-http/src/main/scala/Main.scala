import zhttp.http._
import zhttp.service.{EventLoopGroup, Server}
import zio.{App, ExitCode, URIO}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import zhttp.http.Response
import zhttp.service.server.ServerChannelFactory

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

case class Message(message: String)

object Main extends App {
  val message: String                         = "Hello, World!"
  val messageLength: Long = message.size
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make
  val plaintextResp = Response(data= HttpData.fromText(message), headers = Header.contentTypeTextPlain :: headers())
  val jsonResp = Response(data = HttpData.fromText(writeToString(Message(message))), headers = Header.contentTypeJson :: headers())

  val app = HttpApp.collect{
    case Method.GET -> !! / "plaintext" => plaintextResp
    case Method.GET -> !! / "json"      => jsonResp

  }
  val server = Server.app(app.silent) ++ Server.port(8080) ++ Server.keepAlive ++ Server.disableLeakDetection

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = server.make.useForever.provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto()).exitCode

  val formatter: DateTimeFormatter                = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)
  val constantHeaders: List[Header]               = Header("server", "zio-http") :: Nil
  var lastHeaders: (Long, List[Header]) = (0, Nil)

  def headers(): List[Header] = {
    val t = System.currentTimeMillis()
    if (t - lastHeaders._1 >= 1000)
      lastHeaders = (t, Header("date", formatter.format(Instant.ofEpochMilli(t))) :: constantHeaders)
    lastHeaders._2
  }
}
