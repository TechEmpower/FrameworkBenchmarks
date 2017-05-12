package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}


class JsonHandler(components: {

}) {

  import JsonHandler.Protocols._

  def endpoint = get {
    path("json") {
      complete(response)
    }
  }

  def response = Response("Hello, World!")
}

object JsonHandler {

  object Protocols extends DefaultJsonProtocol with SprayJsonSupport {

    case class Response(message: String)

    implicit val responseFormat: RootJsonFormat[Response] = jsonFormat1(Response.apply)

  }

}