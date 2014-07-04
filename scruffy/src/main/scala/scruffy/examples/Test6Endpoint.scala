package scruffy.examples

import com.sksamuel.scruffy.EndpointProvider
import com.sksamuel.scruffy.http.MediaType

/** @author Stephen Samuel */
class Test6Endpoint extends EndpointProvider {

  get("plaintext").complete {
    req => entity("Hello, World!", MediaType.TextPlain)
  }

}
