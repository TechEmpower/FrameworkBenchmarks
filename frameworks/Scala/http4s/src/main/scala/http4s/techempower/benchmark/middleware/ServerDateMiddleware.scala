package http4s.techempower.benchmark.middleware

import java.time.Instant

import cats.data.Kleisli
import cats.effect.Effect
import cats.implicits._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.{Header, HttpService, Request, Status}

final class ServerDateMiddleware[F[_]: Effect](service: HttpService[F]) {

  val middleware: HttpService[F] = Kleisli { req: Request[F] =>
    service(req).map {
      case Status.Successful(resp) =>
        resp.putHeaders(
          Header("Server", req.serverAddr),
          Header("Date", Instant.now.toString)
        )
      case r => r
    }
  }

}
