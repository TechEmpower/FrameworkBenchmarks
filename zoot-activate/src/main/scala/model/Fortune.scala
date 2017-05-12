package model

import persistenceContext._

class Fortune(val id: Int, val message: String) extends EntityWithCustomID[Int]

object Fortune {
    def all =
        asyncTransactionalChain { implicit ctx =>
            asyncQuery {
                (fortune: Fortune) => where() select (fortune.eager)
            }
        }
}