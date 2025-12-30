package pekko.http.benchmark

import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import pekko.http.benchmark.handlers.{DbHandler, FortunesHandler, JsonHandler, QueriesHandler, UpdatesHandler}

trait RequestMapping { _: JsonHandler with DbHandler with QueriesHandler with FortunesHandler with UpdatesHandler =>
  def asRoute: Route =
    jsonEndpoint ~ dbEndpoint ~ queriesEndpoint ~ fortunesEndpoint ~ updatesEndpoint
}
