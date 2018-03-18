package http4s.techempower.benchmark.service

import java.util.concurrent.ThreadLocalRandom

import cats._
import cats.implicits._
import cats.effect.Effect
import doobie.implicits._
import doobie.util.transactor.Transactor
import http4s.techempower.benchmark.model.World

import scala.annotation.tailrec

final class DatabaseService[F[_]](val xa: Transactor[F]) {

  // Select a World object from the database by ID
  def selectRandomWorldId(implicit F: Effect[F]): F[World] =
    sql"select id, randomNumber from World where id = ${ThreadLocalRandom.current
      .nextInt(1, 10001)}"
      .query[World]
      .unique
      .transact(xa)

  // TODO: parallelize
  def selectNWorlds(i: Int)(implicit F: Effect[F]): F[List[World]] = {
    // Kmett forgive me for i have sinned
    var t = F.pure(Nil: List[World])
    var k = 1
    while (k <= i) {
      t = (selectRandomWorldId, t).mapN(_ :: _)
      k += 1
    }
    t
  }

}
