package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.Config

trait Bootstrap {
  def run(): Unit
}

class BenchmarkBootstrap(components: {
  val config: Config
  val system: ActorSystem
  val route: Route
}) extends Bootstrap {
  implicit val system = components.system
  val config = components.config

  import system.dispatcher

  override def run(): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(components.route, config.getString("akka.http.benchmark.host"), config.getInt("akka.http.benchmark.port"))
  }
}