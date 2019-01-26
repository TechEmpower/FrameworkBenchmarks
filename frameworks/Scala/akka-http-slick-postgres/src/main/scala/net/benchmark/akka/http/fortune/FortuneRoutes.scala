package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives.{pathEnd, post}
import akka.http.scaladsl.server.Route
import net.benchmark.akka.http.fortune.FortuneRepository

import scala.concurrent.ExecutionContext

class FortuneRoutes(fr: FortuneRepository, ec: ExecutionContext) {

  implicit private val routesExecutionContext: ExecutionContext = ec

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def fortuneRoutes(): Route = {
    pathEnd {
      post {
        createFortune()
      }
    }
  }

  def createFortune(): Route = {
    // fr.create(entity))
    ???
  }

}
