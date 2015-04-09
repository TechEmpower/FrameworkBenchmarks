package scruffy.examples

import com.sksamuel.scruffy.HttpEndpointProvider

/** @author Stephen Samuel */
class Test1Endpoint extends HttpEndpointProvider {

  get("json") { implicit req =>
    json {
      Message("Hello, World!")
    }
  }
}

case class Message(message: String)