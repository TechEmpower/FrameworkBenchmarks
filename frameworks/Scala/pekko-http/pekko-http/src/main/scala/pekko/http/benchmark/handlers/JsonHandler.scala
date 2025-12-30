package pekko.http.benchmark.handlers

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class JsonResponse(message: String)

object JsonResponse {
  implicit val codec: JsonValueCodec[JsonResponse] = JsonCodecMaker.make[JsonResponse](CodecMakerConfig)
}

trait JsonHandler {
  import com.github.pjfanning.pekkohttpjsoniterscala.JsoniterScalaSupport._

  def jsonEndpoint: Route =
    (get & path("json")) {
      complete(JsonResponse("Hello, World!"))
    }
}
