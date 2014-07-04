package scruffy.examples

import com.sksamuel.scruffy.EndpointProvider

/** @author Stephen Samuel */
class Test1Endpoint extends EndpointProvider {

  get("json").complete {
    req => json(Message("Hello World"))
  }
}

case class Message(message: String)