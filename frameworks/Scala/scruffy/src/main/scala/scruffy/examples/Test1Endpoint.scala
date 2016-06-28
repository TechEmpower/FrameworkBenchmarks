package scruffy.examples

import com.sksamuel.scruffy.HttpModule

/** @author Stephen Samuel */
object Test1Endpoint extends HttpModule {

  import com.sksamuel.scruffy.jackson.ScruffyJackson.Implicits._

  get("json") { req =>
    Message("Hello, World!").json
  }
}

case class Message(message: String)