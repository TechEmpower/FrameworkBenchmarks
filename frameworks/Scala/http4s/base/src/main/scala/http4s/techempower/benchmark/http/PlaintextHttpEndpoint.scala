package http4s.techempower.benchmark.http

import cats.effect.Effect
import fs2.{Chunk, Pure, Stream}
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpService, MediaType}
import org.http4s.headers.`Content-Type`

final class PlaintextHttpEndpoint[F[_]: Effect] {

  def service: HttpService[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "plaintext" =>
        Ok(response, `Content-Type`.apply(MediaType.`text/plain`))
    }
  }

  private[this] val response: Stream[Pure, Byte] =
    Stream.chunk(Chunk.bytes("Hello, World!".getBytes("utf-8")))
}
