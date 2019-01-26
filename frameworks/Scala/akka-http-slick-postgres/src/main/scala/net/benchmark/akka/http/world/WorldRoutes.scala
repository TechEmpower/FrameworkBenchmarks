package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives.{pathEnd, post}
import akka.http.scaladsl.server.Route

import scala.concurrent.ExecutionContext

class WorldRoutes(wr: WorldRepository, ec: ExecutionContext) {

  implicit private val routesExecutionContext: ExecutionContext = ec

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def worldRoutes(): Route = {
    pathEnd {
      post {
        createWorld()
      }
    }
  }

  def createWorld(): Route = {
    // wr.create(entity))
    ???
  }

}
