import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import snunit.*

final case class Message(message: String) derives ConfiguredJsonValueCodec

final val applicationJson = Headers("Content-Type" -> "application/json")
final val textPlain = Headers("Content-Type" -> "text/plain")

inline def notFound(req: Request) = req.send(
  statusCode = StatusCode.NotFound,
  content = "Not found",
  headers = textPlain
)

@main
def main =
  SyncServerBuilder
    .setRequestHandler(req =>
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
    .build()
    .listen()
