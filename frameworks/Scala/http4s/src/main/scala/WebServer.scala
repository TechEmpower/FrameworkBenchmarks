import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder

import _root_.argonaut._, Argonaut._

object WebServer extends App {

  val service = HttpService {
    case GET -> Root / "json" =>
      Ok(Json("message" -> jString("Hello, World!")).asJson)
  }

  BlazeBuilder.bindHttp(8080)
    .mountService(service, "/")
    .run
    .awaitShutdown()
}