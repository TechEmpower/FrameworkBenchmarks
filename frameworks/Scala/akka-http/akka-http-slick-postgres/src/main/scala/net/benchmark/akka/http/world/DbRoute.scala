package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives.{complete, path, withExecutionContext}
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

import scala.concurrent.ExecutionContextExecutor

class DbRoute(wr: WorldRepository, dd: ExecutionContextExecutor) {

  private def rand(): Int = {
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  def route() = {
    path("db") {
      withExecutionContext(dd) {
        complete(wr.require(rand()))
      }
    }
  }

}
