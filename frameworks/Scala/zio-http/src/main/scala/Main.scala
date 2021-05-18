import zhttp.http._
import zhttp.service.Server
import zio.{App, ExitCode, URIO}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object WebApp extends App {
  val message: String                         = "Hello, World!"
  def createDate: String                      = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now)
  case class Message(message: String)
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make

  val app = Http.collect[Request] {
    case Method.GET -> Root / "plaintext" => Response.text(message)
    case Method.GET -> Root / "json"      => Response.jsonString(writeToString(Message(message)))
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = Server.start(8080, app).exitCode

}
