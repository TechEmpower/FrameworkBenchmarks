package blaze.techempower.benchmark

import java.net.InetSocketAddress

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import org.http4s.blaze.channel.SocketConnection
import org.http4s.blaze.http._

import scala.concurrent.Future

object Main {

  private val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)

  private val plaintextResult = Future.successful {
    RouteAction.Ok("Hello, World!", Seq("server" -> "blaze"))
  }

  private def notFound(path: String) = Future.successful {
    RouteAction.String(s"Not found: $path", 404, "Not Found", Nil)
  }

  // HTTP service definition
  private def service(request: HttpRequest): Future[RouteAction] = request.uri match {
    case "/plaintext" => plaintextResult

    case "/json" => Future.successful {
      val msg = mapper.writeValueAsBytes(Map("message" -> "Hello, World!"))
      RouteAction.Ok(msg, Seq("server" -> "blaze", "content-type" -> "application/json"))
    }

    case other => notFound(other)
  }

  def main(args: Array[String]): Unit = {
    val srvc = { _: SocketConnection => service(_:HttpRequest) }
    val server = Http1Server(srvc, new InetSocketAddress(8080), HttpServerStageConfig())
      .getOrElse(sys.error("Failed to bind socket"))

    try server.channel.join()
    finally {
      server.group.closeGroup()
    }
  }
}

