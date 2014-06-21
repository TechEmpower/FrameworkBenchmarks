package model

import java.util.concurrent.ThreadLocalRandom

import net.fwbrasil.radon.transaction.TransactionalExecutionContext
import persistenceContext._

class World(val id: Int, var randomNumber: Long) extends EntityWithCustomID[Int]

object World {

    private val TestDatabaseRows = 10000

    def updateOne =
        asyncTransactionalChain { implicit ctx =>
            find(randomId).map { world =>
                world.randomNumber = random.nextInt(TestDatabaseRows) + 1
                world
            }
        }

    def pickOne =
        asyncTransactionalChain { implicit ctx =>
            find(randomId)
        }

    private def find(id: Int)(implicit ctx: TransactionalExecutionContext) =
        asyncQuery {
            (world: World) => where(world.id :== id) select (world.eager)
        }.map {
            _.headOption.getOrElse(throw new IllegalStateException("invalid world id " + id))
        }

    private def random = ThreadLocalRandom.current
    private def randomId = random.nextInt(TestDatabaseRows) + 1
}
