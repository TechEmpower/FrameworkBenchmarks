package vertx.model

import io.vertx.lang.scala.json.JsonObject

object Message {
  private val MESSAGE = "message"
}

case class Message(message: String) extends JsonObject {
  put(Message.MESSAGE, message)
}
