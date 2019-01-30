package net.benchmark.akka.http.fortune

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

import scala.concurrent.ExecutionContextExecutor

class FortuneRoutes(fr: FortuneRepository, fd: ExecutionContextExecutor) {

  implicit private val jss: JsonEntityStreamingSupport =
    EntityStreamingSupport.json().withParallelMarshalling(5, unordered = false)

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def routes(): Route = {
    path("fortunes") {
      withExecutionContext(fd) {
        complete(Source.fromPublisher(fr.all()))
      }
    }
  }

}
