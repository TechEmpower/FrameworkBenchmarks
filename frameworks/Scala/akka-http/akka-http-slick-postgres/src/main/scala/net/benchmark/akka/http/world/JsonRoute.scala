package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives.{complete, path}
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

object JsonRoute {

  private val msg = "Hello, World!"

  def route: Route = {
    path("json") {
      complete(Hello(msg))
    }
  }

}
