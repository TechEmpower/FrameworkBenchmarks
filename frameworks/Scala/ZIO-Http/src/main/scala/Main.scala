import zhttp.http._
import zhttp.service.Server
import zio.{App, ExitCode, URIO}

object WebApp extends App {

  val message: String = "Hello, World!"

  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import com.github.plokhotnyuk.jsoniter_scala.core._

  case class Message(message: String)
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make

  val jsonstr = new String(writeToArray(Message(message)))

  val app = Http.collect[Request] {
    case Method.GET -> Root / "plaintext" => Response.text(message)
    case Method.GET -> Root / "json" => Response.jsonString(jsonstr)
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = Server.start(8080, app).exitCode

}