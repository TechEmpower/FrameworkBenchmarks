package scruffy.examples

import com.sksamuel.scruffy.HttpModule
import com.sksamuel.scruffy.http.MediaType

/** @author Stephen Samuel */
object Test6Endpoint extends HttpModule {

  get("plaintext") { req =>
    ok("Hello, World!", MediaType.TextPlain)
  }
}
