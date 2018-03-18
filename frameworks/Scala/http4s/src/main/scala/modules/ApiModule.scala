package modules

import cats.effect.Effect
import doobie.util.transactor.Transactor
import http.{DatabaseHttpEndpoint, JsonHttpEndpoint}
import middleware.ServerDateMiddleware
import org.http4s.HttpService
import service.DatabaseService
import cats.implicits._

final class ApiModule[F[_]: Effect] {

  private[this] val xa = Transactor.fromDriverManager[F](
    "org.postgresql.Driver", // Driver
    "jdbc:postgresql:world", // Url
    "benchmarkdbuser", // User
    "benchmarkdbpass" // Pass
  )

  private[this] val databaseService: DatabaseService[F] =
    new DatabaseService[F](xa)

  private[this] val jsonHttpEndpoint: HttpService[F] =
    new JsonHttpEndpoint[F].service

  private[this] val databaseHttpEndpoint: HttpService[F] =
    new DatabaseHttpEndpoint[F](databaseService).service

  private[this] val headerMiddleware: ServerDateMiddleware[F] =
    new ServerDateMiddleware[F]

  val api: HttpService[F] =
    headerMiddleware.addServerDateHeaders(jsonHttpEndpoint) <+>
      headerMiddleware.addServerDateHeaders(databaseHttpEndpoint)
}
