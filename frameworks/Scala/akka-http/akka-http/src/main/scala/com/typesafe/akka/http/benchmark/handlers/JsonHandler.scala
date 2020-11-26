package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class JsonResponse(message: String)

object JsonResponse {
  implicit val codec: JsonValueCodec[JsonResponse] = JsonCodecMaker.make[JsonResponse](CodecMakerConfig)
}

trait JsonHandler {
  import de.heikoseeberger.akkahttpjsoniterscala.JsoniterScalaSupport._

  def jsonEndpoint: Route =
    (get & path("json")) {
      complete(JsonResponse("Hello, World!"))
    }
}
