package middleware

import java.time.Instant

import cats.data.Kleisli
import cats.effect.Effect
import org.http4s.dsl.Http4sDsl
import org.http4s.{Header, HttpService, Request, Status}

final class ServerDateMiddleware[F: Effect] extends Http4sDsl[F] {

  def addServerDateHeaders(service: HttpService[F]): HttpService[F] = Kleisli { req: Request[F] =>
    service(req).map {
      case Status.Successful(resp) =>
        resp.putHeaders(
          Header("Server", req.serverAddr),
          Header("Date", Instant.now.toString)
        )
      case resp => resp
    }
  }
}
