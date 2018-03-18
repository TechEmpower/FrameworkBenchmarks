package http

import cats.Monad
import cats.effect.Effect
import cats.implicits._
import org.http4s.{HttpService, MediaType}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Type`, `Content-Length`}
import io.circe.literal._

final class JsonHttpEndpoint[F[_]: Effect] extends Http4sDsl[F] {

  def service(implicit F: Monad[F]): HttpService[F] = HttpService[F] {
    case GET -> Root / "json" =>
      for {
        ok <- Ok(json"""{"message":"Hello, World!"}""", `Content-Length`.unsafeFromLong(28L))
        r  <- ok.withContentType(`Content-Type`.apply(MediaType.`application/json`))
      } yield r
  }
}
