package vertx.model

import io.vertx.lang.scala.json.JsonObject

object World {
  private val ID = "id"
  private val RANDOM_NUMBER = "randomNumber"
}

case class World(id: Int, randomNumber: Int) extends JsonObject with Ordered[World] {
  put(World.ID, id)
  put(World.RANDOM_NUMBER, randomNumber)

  override def compare(o: World): Int = Integer.compare(id, o.id)
}
