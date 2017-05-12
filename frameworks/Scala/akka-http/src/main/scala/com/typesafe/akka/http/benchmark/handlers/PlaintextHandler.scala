package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._

trait PlaintextHandler {
  def plainTextEndpoint =
    get {
      path("plaintext") {
        complete("Hello, World!")
      }
    }
}
