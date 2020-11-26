package example

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.youi.http.content._
import io.youi.net._
import io.youi.server._
import io.youi.server.dsl._

case class Message(message: String)

object Message {
  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make
}

object Main {
  def main(args: Array[String]): Unit = {
    val server = new UndertowServerImplementation(new Server {
      handler(
        filters(
          path"/plaintext" / StringContent("Hello, World!", ContentType.`text/plain`),
          path"/json" / BytesContent(writeToArray(Message("Hello, World!")), ContentType.`application/json`)
        )
      )
      config.clearListeners().addHttpListener("0.0.0.0")
    })
    server.start()
  }
}