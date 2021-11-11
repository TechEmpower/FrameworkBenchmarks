package example

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.undertow.{Undertow, UndertowOptions}

case class Message(message: String)

object Main extends cask.MainRoutes {

  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make[Message](CodecMakerConfig())

  override def main(args: Array[String]): Unit = {
    val server = Undertow.builder
      .addHttpListener(8080, "0.0.0.0")
      // increase io thread count as per https://github.com/TechEmpower/FrameworkBenchmarks/pull/4008
      .setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
      // In HTTP/1.1, connections are persistent unless declared otherwise.
      // Adding a "Connection: keep-alive" header to every response would only
      // add useless bytes.
      .setServerOption[java.lang.Boolean](UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
      .setHandler(defaultHandler)
      .build
    server.start()
  }

  @cask.get("/plaintext")
  def plaintext() = {
    cask.Response(data = "Hello, World!",
      headers = Seq("Server" -> "cask", "Content-Type" -> "text/plain")
    )

  }

  @cask.get("/json")
  def json(request: cask.Request) = {
    cask.Response(data = writeToArray(Message("Hello, World!")),
      headers = Seq("Server" -> "cask", "Content-Type" -> "application/json")
    )
  }

  initialize()
}
