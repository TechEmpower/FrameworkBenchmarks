import com.twitter.finagle.Http
import com.twitter.finagle.http.Request
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finatra.http.filters.HttpResponseFilter
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}

object FinatraBenchmarkServerMain extends FinatraBenchmarkServer

class FinatraBenchmarkServer extends HttpServer {

  override protected def configureHttpServer(server: Http.Server): Http.Server = {
    server
      .configured(Http.Netty3Impl)
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withStack(Http.server.stack.tails.toSeq.last)
  }

  override def configureHttp(router: HttpRouter): Unit = {
    router
      .filter[HttpResponseFilter[Request]]
      .add[FinatraBenchmarkController]
  }
}

class FinatraBenchmarkController extends Controller {
  get("/plaintext") { request: Request =>
    "Hello, World!"
  }

  get("/json") { request: Request =>
    Map("message" -> "Hello, World!")
  }
}