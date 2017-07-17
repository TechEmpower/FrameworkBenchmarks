package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit = {
    val routeHandler = Route.asyncHandler(asRoute)

    Http().bindAndHandleAsync(
      routeHandler,
      appConfig.getString("akka.http.benchmark.host"),
      appConfig.getInt("akka.http.benchmark.port"),
      parallelism = 16)
  }
}