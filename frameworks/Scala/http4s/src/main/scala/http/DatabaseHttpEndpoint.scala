package http

import cats.{Monad, Show}
import cats.implicits._
import cats.effect.Effect
import org.http4s.{HttpService, MediaType}
import org.http4s.dsl.Http4sDsl
import service.DatabaseService
import implicits._
import model.World
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
