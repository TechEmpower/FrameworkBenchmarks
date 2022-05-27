package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.handlers._

trait RequestMapping { _: JsonHandler with DbHandler with QueriesHandler with FortunesHandler with UpdatesHandler =>
  def asRoute: Route =
    jsonEndpoint ~ dbEndpoint ~ queriesEndpoint ~ fortunesEndpoint ~ updatesEndpoint
}
