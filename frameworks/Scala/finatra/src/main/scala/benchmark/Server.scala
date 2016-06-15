package benchmark

import benchmark.controllers.Controller
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.HttpServer
import com.twitter.finatra.http.filters.HttpResponseFilter
import com.twitter.finatra.http.routing.HttpRouter

object ServerMain extends Server

class Server extends HttpServer {

  override def configureHttp(router: HttpRouter): Unit = {
	router
		.filter[HttpResponseFilter[Request]]
		.add[Controller]
  }
}
