package vertx.model

import io.vertx.lang.scala.json.JsonObject

object Fortune {
  private val ID = "id"
  private val MESSAGE = "message"
}

case class Fortune(id: Int, message: String) extends JsonObject with Ordered[Fortune] {
  put(Fortune.ID, id)
  put(Fortune.MESSAGE, message)

  override def compare(other: Fortune): Int = message.compareTo(other.message)
}
