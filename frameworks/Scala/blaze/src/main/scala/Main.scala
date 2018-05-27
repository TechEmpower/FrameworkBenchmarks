package blaze.techempower.benchmark

import java.lang.Runtime._
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousChannelGroup
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinPool._

import org.http4s.blaze.channel.nio2.NIO2SocketServerGroup
import org.http4s.blaze.http._
import org.http4s.blaze.http.HttpServerStageConfig
import org.http4s.blaze.http.http1.server.Http1ServerStage
import org.http4s.blaze.pipeline.LeafBuilder
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import org.http4s.blaze.channel.SocketConnection
import org.http4s.blaze.http.RouteAction._

import scala.concurrent.Future

case class Message(message: String)

object Main {
  private val config = HttpServerStageConfig()
  private val fjp = new ForkJoinPool(getRuntime.availableProcessors, defaultForkJoinWorkerThreadFactory, null, true)
  private val jsonHeaders = Seq("server" -> "blaze", "content-type" -> "text/plain")
  private val plaintextHeaders = Seq("server" -> "blaze", "content-type" -> "application/json")

  private implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make[Message](CodecMakerConfig())

  def serve(request: HttpRequest): Future[RouteAction] = Future.successful {
    request.url match {
      case "/plaintext" => Ok("Hello, World!".getBytes(UTF_8), plaintextHeaders)
      case "/json" => Ok(writeToArray(Message("Hello, World!")), jsonHeaders)
    }
  }

  def connect(conn: SocketConnection): Future[LeafBuilder[ByteBuffer]] =
    Future.successful(LeafBuilder(new Http1ServerStage(serve, config)))

  def main(args: Array[String]): Unit = {
    NIO2SocketServerGroup(group = Some(AsynchronousChannelGroup.withThreadPool(fjp)))
      .bind(new InetSocketAddress(8080), connect)
      .getOrElse(sys.error("Failed to start server."))
      .join()
  }
}
