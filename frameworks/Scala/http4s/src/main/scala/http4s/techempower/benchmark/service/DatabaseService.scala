package http4s.techempower.benchmark.service

import java.util.concurrent.ThreadLocalRandom

import cats.effect._
import cats._
import cats.implicits._
import doobie.implicits._
import doobie.imports.Update
import doobie.util.transactor.Transactor
import http4s.techempower.benchmark.model.World

final class DatabaseService[F[_]: Effect](xa: Transactor[F]) {

  // Reuse random by >>=
  val random: F[Int] =
    Effect[F].delay(ThreadLocalRandom.current.nextInt(1, 10001))

  // Select a World object from the database by ID
  @inline def selectRandomWorldId(id: Int): F[World] =
    sql"select id, randomNumber from World where id = $id"
      .query[World]
      .unique
      .transact(xa)

  @inline def selectNWorlds(i: Int): F[List[World]] = {
    // Kmett forgive me for i have sinned
    var t = Effect[F].pure(List.empty[World])
    var k = 1
    while (k <= i) {
      t = (random >>= (id => selectRandomWorldId(id)), t).mapN(_ :: _)
      k += 1
    }
    t
  }

  @inline def selectNDifferentWorlds(ws: List[World]): F[List[World]] =
    ws.map(w => random.map(id => w.copy(id = id))).sequence

  @inline def updateNWorlds(ws: List[World]): F[Int] =
    Update[(Int, Int)]("update World set randomNumber = ? where id = ?")
      .updateMany(ws.map(w => (w.randomNumber, w.id)))
      .transact(xa)
}
