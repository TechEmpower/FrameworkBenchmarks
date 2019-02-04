package net.benchmark.akka.http.world
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

import scala.concurrent.ExecutionContextExecutor

class WorldRoutes(wr: WorldRepository,
                  sd: ExecutionContextExecutor,
                  qd: ExecutionContextExecutor,
                  ud: ExecutionContextExecutor,
                  dd: ExecutionContextExecutor) {

  private val qr = new QueriesRoute(wr, qd).route()
  private val ur = new UpdateRoute(wr, ud, sd).route()
  private val dr = new DbRoute(wr, dd).route()

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def routes(): Route = {
    qr ~ ur ~ dr ~ JsonRoute.route ~ PlainTextRoute.route
  }

}
