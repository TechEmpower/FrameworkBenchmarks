package benchmark.controllers

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http.{Controller => HttpController}
import com.twitter.io.Buf
import com.twitter.util.Future

class Controller extends HttpController {
  private[this] val helloWorldText = "Hello, World!"
  private[this] val helloWorldBuf: Buf = Buf.Utf8(helloWorldText)

  get("/json") { request: Request =>
    Map("message" -> helloWorldText)
  }

  get("/plaintext") { request: Request =>
    val resp = Response()
    resp.content = helloWorldBuf
    resp.contentType = response.plainTextContentType
    Future.value(resp)
  }
}
