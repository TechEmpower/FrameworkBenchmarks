package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._

class PlaintextHandler(components: {

}) {
  def endpoint = get {
    path("plaintext") {
      complete(response)
    }
  }

  def response = new HttpResponse(StatusCodes.OK, entity = HttpEntity("Hello, World!").withContentType(`text/plain`.withCharset(`UTF-8`)))
}
