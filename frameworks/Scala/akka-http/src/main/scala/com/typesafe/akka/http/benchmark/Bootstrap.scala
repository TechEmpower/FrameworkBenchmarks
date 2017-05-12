package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.Config

trait Bootstrap {
  def run(): Unit
}

trait BenchmarkBootstrap extends Bootstrap { _: Infrastructure with RequestMapping =>
  override def run(): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(asRoute, appConfig.getString("akka.http.benchmark.host"), appConfig.getInt("akka.http.benchmark.port"))
  }
}