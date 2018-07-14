package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit =
    Http().bindAndHandleAsync(
      Route.asyncHandler(asRoute),
      appConfig.getString("akka.http.benchmark.host"),
      appConfig.getInt("akka.http.benchmark.port"),
      parallelism = 16)
}