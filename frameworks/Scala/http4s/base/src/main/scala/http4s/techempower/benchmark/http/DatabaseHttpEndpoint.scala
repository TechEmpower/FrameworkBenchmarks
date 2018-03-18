package http4s.techempower.benchmark.http

import cats.{Monad, Show}
import cats.implicits._
import cats.effect.Effect
import http4s.techempower.benchmark.model.World
import http4s.techempower.benchmark.service.DatabaseService
import http4s.techempower.benchmark.implicits._
import org.http4s.{HttpService, MediaType}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`

final class DatabaseHttpEndpoint[F[_]: Effect](
    databaseService: DatabaseService[F]) {

  def service(implicit F: Monad[F]): HttpService[F] = {
    val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "db" =>
        for {
          w <- databaseService.selectRandomWorldId
          r <- Ok(Show[World].show(w)) >>= (r =>
            F.pure(r.withContentType(
              `Content-Type`.apply(MediaType.`application/json`))))
        } yield r
    }
  }
}
