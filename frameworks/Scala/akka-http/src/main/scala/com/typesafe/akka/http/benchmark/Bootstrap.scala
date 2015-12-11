package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Route, RoutingLog, RoutingSettings}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink._
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

    implicit val routingLog = RoutingLog(system.log)
    implicit val materializer = ActorMaterializer()
    implicit val settings = RoutingSettings.default(system)
    val handler = Route.asyncHandler(components.route)
    val server = Http(components.system).bind(config.getString("akka.http.benchmark.host"), config.getInt("akka.http.benchmark.port"))
    server.to {
      foreach { connection =>
        connection.handleWithAsyncHandler(handler)
      }
    }.run()
  }
}