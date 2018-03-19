package http4s.techempower.benchmark.service

import cats.effect.Effect
import doobie.implicits._
import doobie.util.transactor.Transactor
import http4s.techempower.benchmark.model.Fortune
import doobie._

final class FortuneService[F[_]](xa: Transactor[F]) {

  @inline def selectFortune(implicit F: Effect[F]): F[List[Fortune]] =
    sql"select id, message from Fortune"
      .query[Fortune]
      .to[List]
      .transact(xa)

}
