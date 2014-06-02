package scruffy.examples

import com.sksamuel.scruffy.RouteProvider
import com.sksamuel.scruffy.http.MediaType

/** @author Stephen Samuel */
class Test6Route extends RouteProvider {

  get("plaintext").handler {
    req => entity("Hello, World!", MediaType.TextPlain)
  }

}
