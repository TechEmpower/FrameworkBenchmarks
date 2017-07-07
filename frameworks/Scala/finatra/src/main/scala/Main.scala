import com.twitter.finagle.Http.{Netty3Impl, Server}
import com.twitter.finagle.http.Request
import com.twitter.finagle.stack.nilStack
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finatra.http.filters.HttpResponseFilter
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}

object FinatraBenchmarkServerMain extends FinatraBenchmarkServer

class FinatraBenchmarkServer extends HttpServer {
  override def configureHttpServer(server: Server): Server = {
    server
      .configured(Netty3Impl)
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withStack(nilStack)
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