package modules

import cats.effect.Effect
import http.JsonHttpEndpoint
import middleware.ServerDateMiddleware
import org.http4s.HttpService

final class ApiModule[F[_]: Effect] {

  private[this] val jsonHttpEndpoint: HttpService[F] =
    new JsonHttpEndpoint[F].service

  private[this] val headerMiddleware: ServerDateMiddleware[F] =
    new ServerDateMiddleware[F]

  val api: HttpService[F] =
    headerMiddleware.addServerDateHeaders(jsonHttpEndpoint)
}
