package modules

import cats.effect.Effect
import http.JsonHttpEndpoint
import middleware.ServerDateMiddleware
import org.http4s.HttpService

final class ApiModule[F[_]: Effect] {

  val jsonHttpEndpoint: HttpService[F] =
    new JsonHttpEndpoint[F].service

  val headerMiddleware: ServerDateMiddleware[F] =
    new ServerDateMiddleware[F]


}
