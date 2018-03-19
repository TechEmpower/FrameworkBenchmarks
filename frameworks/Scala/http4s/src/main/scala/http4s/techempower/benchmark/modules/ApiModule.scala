package http4s.techempower.benchmark.modules

import cats.effect.Effect
import doobie.util.transactor.Transactor
import org.http4s.HttpService
import cats.implicits._
import http4s.techempower.benchmark.http._
import http4s.techempower.benchmark.middleware.ServerDateMiddleware
import http4s.techempower.benchmark.service.{DatabaseService, FortuneService}

final class ApiModule[F[_]: Effect] {

  // Use the same transactor per service - tested sequentially not concurrently
  private[this] val xa = Transactor.fromDriverManager[F](
    "org.postgresql.Driver", // Driver
    "jdbc:postgresql:world", // Url
    "benchmarkdbuser", // User
    "benchmarkdbpass" // Pass
  )

  // Services
  private[this] val databaseService: DatabaseService[F] =
    new DatabaseService[F](xa)

  private[this] val fortuneService: FortuneService[F] =
    new FortuneService[F](xa)

  // Endpoints
  private[this] val jsonHttpEndpoint: HttpService[F] =
    new JsonHttpEndpoint[F].service

  private[this] val queriesHttpEndpoint: HttpService[F] =
    new QueriesHttpEndpoint[F](databaseService).service

  private[this] val databaseHttpEndpoint: HttpService[F] =
    new DatabaseHttpEndpoint[F](databaseService).service

  private[this] val fortuneHttpEndpoint: HttpService[F] =
    new FortuneHttpEndpoint[F](fortuneService).service

  private[this] val plaintextHttpEndpoint: HttpService[F] =
    new PlaintextHttpEndpoint[F].service

  private[this] val endpoints: HttpService[F] =
    plaintextHttpEndpoint <+>
      jsonHttpEndpoint <+>
      databaseHttpEndpoint <+>
      queriesHttpEndpoint <+>
      fortuneHttpEndpoint

  private[this] val endpointsWithMiddleware: HttpService[F] =
    new ServerDateMiddleware[F](endpoints).middleware

  val api: HttpService[F] = endpointsWithMiddleware
}
