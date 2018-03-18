package http4s.techempower.benchmark.http

import cats.Monad
import cats.effect.Effect
import org.http4s.{HttpService, MediaType}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import fs2.{Chunk, Pure, Stream}
import org.http4s._

final class JsonHttpEndpoint[F[_]: Effect] {

  def service(implicit F: Monad[F]): HttpService[F] = {
    val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "json" =>
        Ok(
          """{"message":"Hello, World!"}""",
          `Content-Length`.unsafeFromLong(28L),
          `Content-Type`.apply(
            MediaType.`application/json`
          )
        )
    }
  }

//  private[this] val response: Stream[Pure, Byte] =
//    Stream.chunk(Chunk.bytes("""{"message":"Hello, World!"}""".getBytes("utf-8")))

}
