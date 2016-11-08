package benchmark.controllers

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.{Controller => HttpController}

class Controller extends HttpController {
  private[this] val helloWorldText = "Hello, World!"

  get("/json") { request: Request =>
    Map("message" -> helloWorldText)
  }

  get("/plaintext") { request: Request =>
    helloWorldText
  }
}
