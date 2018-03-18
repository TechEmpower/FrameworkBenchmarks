package http4s.techempower.benchmark.http

import cats.effect.Effect
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpService, MediaType}
import org.http4s.headers.`Content-Type`
import cats.implicits._

final class PlaintextHttpEndpoint[F[_]: Effect] {

  def service: HttpService[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "plaintext" =>
        Ok("Hello, World!").map(
          _.withContentType(`Content-Type`(MediaType.`text/plain`)))
    }
  }

}
