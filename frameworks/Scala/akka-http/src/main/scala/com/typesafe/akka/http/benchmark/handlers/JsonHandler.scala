package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json.{ DefaultJsonProtocol, RootJsonFormat }

trait JsonHandler {
  import JsonHandler.Protocols._

  val response = Response("Hello, World!")

  def jsonEndpoint =
    get {
      path("json") {
        complete(response)
      }
    }
}

object JsonHandler {

  object Protocols extends DefaultJsonProtocol with SprayJsonSupport {

    case class Response(message: String)

    implicit val responseFormat: RootJsonFormat[Response] = jsonFormat1(Response.apply)

  }

}