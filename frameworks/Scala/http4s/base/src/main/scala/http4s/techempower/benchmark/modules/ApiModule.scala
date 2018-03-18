package http4s.techempower.benchmark.modules

import cats.effect.Effect
import doobie.util.transactor.Transactor
import org.http4s.HttpService
import cats.implicits._
import http4s.techempower.benchmark.http._
import http4s.techempower.benchmark.middleware.ServerDateMiddleware
import http4s.techempower.benchmark.service.DatabaseService

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

  private[this] val queriesHttpEndpoint: HttpService[F] =
    new QueriesHttpEndpoint[F](databaseService).service

  private[this] val databaseHttpEndpoint: HttpService[F] =
    new DatabaseHttpEndpoint[F](databaseService).service

  private[this] val plaintextHttpEndpoint: HttpService[F] =
    new PlaintextHttpEndpoint[F].service

  private[this] val headerMiddleware: ServerDateMiddleware[F] =
    new ServerDateMiddleware[F]

  val api: HttpService[F] =
    headerMiddleware.addServerDateHeaders(jsonHttpEndpoint) <+>
      headerMiddleware.addServerDateHeaders(databaseHttpEndpoint) <+>
      headerMiddleware.addServerDateHeaders(queriesHttpEndpoint) <+>
      headerMiddleware.addServerDateHeaders(plaintextHttpEndpoint)
}
