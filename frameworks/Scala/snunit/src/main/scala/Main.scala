import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import snunit._

final case class Message(message: String)

object Message {
  implicit final val codec: JsonValueCodec[Message] = JsonCodecMaker.make
}

object Main {
  val applicationJson = Seq("Content-Type" -> "application/json")
  val textPlain = Seq("Content-Type" -> "text/plain")

  def main(args: Array[String]): Unit = {
    SyncServerBuilder
      .build(req =>
        @inline def notFound() = req.send(
          statusCode = StatusCode.NotFound,
          content = "Not found",
          headers = textPlain
        )
        if (req.method == Method.GET) {
          if(req.target == "/plaintext")
            req.send(
              statusCode = StatusCode.OK,
              content = "Hello, World!",
              headers = textPlain
            )
          else if(req.target == "/json")
            req.send(
              statusCode = StatusCode.OK,
              content = writeToArray(Message("Hello, World!")),
              headers = applicationJson
            )
          else notFound()
        } else notFound()
      )
      .listen()
  }
}
