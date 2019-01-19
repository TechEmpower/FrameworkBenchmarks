package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.model.HttpCharsets._
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

trait PlaintextHandler {
  // akka-http will always generate a charset parameter for text/plain, so to be competitive, we create a custom
  // one here to save a few bytes of headers for this particular test case. This is explicitly allowed in:
  // http://frameworkbenchmarks.readthedocs.org/en/latest/Project-Information/Framework-Tests/#specific-test-requirements
  val plainTextResponse = HttpResponse(entity = HttpEntity(MediaType.customWithFixedCharset("text", "plain", `UTF-8`), "Hello, World!"))

  def plainTextEndpoint: Route =
    (get & path("plaintext")) {
      complete(plainTextResponse)
    }
}
