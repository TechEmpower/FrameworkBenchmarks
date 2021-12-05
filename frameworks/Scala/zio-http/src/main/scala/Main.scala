import zhttp.http._
import zhttp.service.{EventLoopGroup, Server}
import zio.{App, ExitCode, URIO}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import io.netty.buffer.Unpooled
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.util.AsciiString
import zhttp.http.Response
import zhttp.service.server.ServerChannelFactory


case class Message(message: String)

object Main extends App {
  val message: String                         = "Hello, World!"

  import io.netty.util.CharsetUtil

  private val STATIC_PLAINTEXT     = message.getBytes(CharsetUtil.UTF_8)
  private val STATIC_SERVER_NAME   = AsciiString.cached("zio-http")
  private val STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length

  private val PLAINTEXT_CLHEADER_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN))
  // Create HTTP route
  val app: HttpApp[Any, Nothing] = Http.response(Response.text(message).withServerTime.memoize.addHeader(HttpHeaderNames.SERVER, STATIC_SERVER_NAME))
  val server = Server.app(app) ++
    Server.port(8080) ++
    Server.keepAlive ++
    Server.disableLeakDetection



  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    server.make.useForever.provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(8)).exitCode
  }

}
