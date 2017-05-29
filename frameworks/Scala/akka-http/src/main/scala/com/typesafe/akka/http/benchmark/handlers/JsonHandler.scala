package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._
import spray.json.DefaultJsonProtocol
import spray.json.RootJsonFormat

case class JsonResponse(message: String)
object JsonResponse {
  import DefaultJsonProtocol._
  implicit val responseFormat: RootJsonFormat[JsonResponse] = jsonFormat1(JsonResponse.apply)
}

trait JsonHandler {
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

  def jsonResponse = JsonResponse("Hello, World!") // domain object

  def jsonEndpoint =
    get {
      path("json") {
        complete(jsonResponse)
      }
    }
}
