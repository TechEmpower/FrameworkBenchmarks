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

  @inline
  private def notFound(req: Request) = req.send(
    statusCode = StatusCode.NotFound,
    content = "Not found",
    headers = textPlain
  )

  def main(args: Array[String]): Unit = {
    SyncServerBuilder
      .build(req =>
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
          else notFound(req)
        } else notFound(req)
      )
      .listen()
  }
}
