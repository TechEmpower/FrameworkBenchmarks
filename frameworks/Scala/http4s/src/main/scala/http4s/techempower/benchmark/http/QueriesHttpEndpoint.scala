package http4s.techempower.benchmark.http

import cats.{Monad, Show}
import cats.implicits._
import cats.effect.Effect
import http4s.techempower.benchmark.model.World
import http4s.techempower.benchmark.service.DatabaseService
import http4s.techempower.benchmark.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Type`, `Content-Length`}
import org.http4s._
import implicits._

final class QueriesHttpEndpoint[F[_]: Effect](
    databaseService: DatabaseService[F]) {

  def service(implicit F: Monad[F]): HttpService[F] = {
    val dsl: Http4sDsl[F] = new Http4sDsl[F] {}
    import dsl._
    HttpService[F] {
      case GET -> Root / "queries" / IntVar(query) =>
        for {
          q <- databaseService.selectNWorlds(normalize(query))
          r <- Ok(q,
                  `Content-Length`.unsafeFromLong(q.size.toLong),
                  `Content-Type`.apply(MediaType.`application/json`))
        } yield r
    }
  }

  @inline private[this] def normalize(i: Int): Int =
    if (i < 1) 1
    else if (i > 500) 500
    else i
}
