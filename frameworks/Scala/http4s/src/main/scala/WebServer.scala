import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder
import headers._

import _root_.argonaut._, Argonaut._

object Middleware {
  def addHeaders(service: HttpService): HttpService = {
    Service.lift { req: Request =>
      service.map { resp =>
        resp.putHeaders(
          headers.Date(DateTime.now),
          Header("Server", req.serverName)
        )
      }(req)
    }
  }
}

object WebServer extends App {

  val service = HttpService {
    case GET -> Root / "json" =>
      Ok(Json("message" -> jString("Hello, World!")))

    case GET -> Root / "plaintext" =>
      Ok("Hello, World!")
        .withContentType(Some(`Content-Type`(MediaType.`text/plain`)))
  }

  BlazeBuilder.bindHttp(8080, "0.0.0.0")
    .mountService(Middleware.addHeaders(service), "/")
    .run
    .awaitShutdown()
}
