package benchmark

import benchmark.controllers.Controller
import com.twitter.finagle.Http
import com.twitter.finagle.http.Request
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.finatra.http.HttpServer
import com.twitter.finatra.http.filters.HttpResponseFilter
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.util.NullMonitor

object ServerMain extends Server

class Server extends HttpServer {

  override protected def configureHttpServer(server: Http.Server): Http.Server = {
    server
      .configured(Http.Netty3Impl)
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .withMonitor(NullMonitor)
  }

  override def configureHttp(router: HttpRouter): Unit = {
    router
      .filter[HttpResponseFilter[Request]]
      .add[Controller]
  }
}
