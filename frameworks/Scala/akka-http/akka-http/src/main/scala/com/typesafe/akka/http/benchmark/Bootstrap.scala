package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.Http

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit =
    Http().newServerAt(
      appConfig.getString("akka.http.benchmark.host"),
      appConfig.getInt("akka.http.benchmark.port"))
      .adaptSettings(settings => settings.mapHttp2Settings(_.withMaxConcurrentStreams(16)))
      .bind(asRoute)
}