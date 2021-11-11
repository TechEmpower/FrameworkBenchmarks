package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives.{complete, path}
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._

class DbRoute(wr: WorldRepository) {

  private def rand(): Int = {
    java.util.concurrent.ThreadLocalRandom.current().nextInt(10000) + 1
  }

  def route() = {
    path("db") {
      complete(wr.require(rand()))
    }
  }

}
