package service

import java.util.concurrent.ThreadLocalRandom

import cats.effect._
import doobie.implicits._
import doobie.util.transactor.Transactor
import model.World

final class DatabaseService[F[_]: Effect](xa: Transactor[F]) {

  // Select a World object from the database by ID
  def selectRandomWorldId: F[World] =
    sql"select id, randomNumber from World where id = ${ThreadLocalRandom.current
      .nextInt(1, 10001)}"
      .query[World]
      .unique
      .transact(xa)
}
