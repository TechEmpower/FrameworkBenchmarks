package scruffy.examples

import com.sksamuel.scruffy.RouteProvider
import com.sksamuel.scruffy.http.MediaType

/** @author Stephen Samuel */
class Test1Route extends RouteProvider {

  get("json").handler {
    req => entity(Message("Hello World"), MediaType.ApplicationJson)
  }
}

case class Message(message: String)