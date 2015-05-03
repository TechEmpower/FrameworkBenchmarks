package scruffy.examples

import com.sksamuel.scruffy.HttpEndpointProvider
import com.sksamuel.scruffy.http.MediaType

/** @author Stephen Samuel */
class Test6Endpoint extends HttpEndpointProvider {

  get("plaintext") { req =>
    ok("Hello, World!", MediaType.TextPlain)
  }
}
