package http4s.techempower.benchmark

import java.util.concurrent.{Executor, ThreadLocalRandom}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import cats.effect.{IO => CatsIO}
import cats.syntax.all._
import io.getquill._

class DatabaseService(ctx: PostgresJAsyncContext[LowerCase.type], executor: Executor) {
  implicit val dbExecutionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)
  import ctx._

  def close(): CatsIO[Unit] = {
    CatsIO(ctx.close())
  }

  // Provide a random number between 1 and 10000 (inclusive)
  private def randomWorldId() =
    CatsIO(ThreadLocalRandom.current().nextInt(1, 10001))

  // Update the randomNumber field with a random number
  def updateRandomNumber(world: World): CatsIO[World] =
    for {
      randomId <- randomWorldId()
    } yield world.copy(randomNumber = randomId)

  // Select a World object from the database by ID
  def selectWorld(id: Int): CatsIO[World] =
    CatsIO.fromFuture(
      CatsIO.delay(
        ctx
          .run(quote {
            query[World].filter(_.id == lift(id))
          })
          .map(rq => rq.head)
      )
    )

  // Select a random World object from the database
  def selectRandomWorld(): CatsIO[World] =
    for {
      randomId <- randomWorldId()
      world <- selectWorld(randomId)
    } yield world

  // Select a specified number of random World objects from the database
  def getWorlds(numQueries: Int): CatsIO[List[World]] =
    (0 until numQueries).toList.traverse(_ => selectRandomWorld())

  // Update the randomNumber field with a new random number, for a list of World objects
  def getNewWorlds(worlds: List[World]): CatsIO[List[World]] =
    worlds.map(updateRandomNumber).sequence

  // Update the randomNumber column in the database for a specified set of World objects,
  // this uses a batch update SQL call.
  def updateWorlds(newWorlds: List[World]): CatsIO[Int] = {
    val u = quote {
      liftQuery(newWorlds).foreach { world =>
        query[World]
          .filter(_.id == world.id)
          .update(_.randomNumber -> world.randomNumber)
      }
    }
    CatsIO.fromFuture(CatsIO.delay(ctx.run(u).map(_.length)))
  }

  // Retrieve all fortunes from the database
  def getFortunes(): CatsIO[List[Fortune]] =
    CatsIO.fromFuture(CatsIO.delay(ctx.run(query[Fortune]).map(_.toList)))
}
