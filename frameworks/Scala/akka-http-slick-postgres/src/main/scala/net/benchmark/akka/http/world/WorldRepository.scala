package net.benchmark.akka.http.world

import scala.concurrent.Future

trait WorldRepository {

  def find(id: Int): Future[Option[World]]

  def require(id: Int): Future[World]

  /**
    *  Update.
    * @param world The new data.
    * @return How many worlds have been updated, usually 1.
    */
  def update(world: World): Future[Int]

}
