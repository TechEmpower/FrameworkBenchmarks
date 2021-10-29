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

  val app: Http[Any, HttpError, Request, Response] = Http.collect[Request] {
    case Method.GET -> Root / "plaintext" =>
      Response.http(
        content = HttpContent.Complete(message),
        headers = Header.contentTypeTextPlain :: headers(),
      )
    case Method.GET -> Root / "json"      =>
      Response.http(
        content = HttpContent.Complete(writeToString(Message(message))),
        headers = Header.contentTypeJson :: headers(),
      )
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = Server.start(8080, app).exitCode

  val formatter: DateTimeFormatter                = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)
  val constantHeaders: List[Header]               = Header("server", "zio-http") :: Nil
  @volatile var lastHeaders: (Long, List[Header]) = (0, Nil)

  def headers(): List[Header] = {
    val t = System.currentTimeMillis()
    if (t - lastHeaders._1 >= 1000)
      lastHeaders = (t, Header("date", formatter.format(Instant.ofEpochMilli(t))) :: constantHeaders)
    lastHeaders._2
  }
}
