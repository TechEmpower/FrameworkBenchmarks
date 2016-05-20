package benchmark.controllers

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.{Controller => HttpController}

class Controller extends HttpController {
  get("/json") { request: Request =>
    Map("message" -> "Hello, World!")
  }

  get("/plaintext") { request: Request =>
    "Hello, World!"
  }
}
