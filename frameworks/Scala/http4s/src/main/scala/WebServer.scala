import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder
import headers._

import _root_.argonaut._, Argonaut._

object WebServer extends App {

  val service = HttpService {
    case GET -> Root / "json" =>
      val dateHeader = Date(DateTime(4))
      Ok(Json("message" -> jString("Hello, World!")).asJson)
        .withHeaders(dateHeader)
        .withContentType(Some(`Content-Type`(MediaType.`application/json`)))
    case GET -> Root / "plaintext" =>
      val dateHeader = Date(DateTime(4))
      Ok("Hello, World!")
        .withHeaders(dateHeader)
        .withContentType(Some(`Content-Type`(MediaType.`text/plain`)))
  }

  BlazeBuilder.bindHttp(8080, "0.0.0.0")
    .mountService(service, "/")
    .run
    .awaitShutdown()
}