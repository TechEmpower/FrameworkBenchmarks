import zhttp.http._
import zhttp.service.Server
import zio.{App, ExitCode, URIO}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import zhttp.http.Response

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

case class Message(message: String)

object Main extends App {
  val message: String                         = "Hello, World!"
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make

  val app = HttpApp.collect{
    case Method.GET -> !! / "plaintext" =>
      Response(data= HttpData.fromText(message), headers = Header.contentTypeJson :: headers())
    case Method.GET -> !! / "json"      =>
      Response(
        data = HttpData.fromText(writeToString(Message(message))),
        headers = Header.contentTypeJson :: headers(),
      )
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = Server.start(8080, app).exitCode

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
