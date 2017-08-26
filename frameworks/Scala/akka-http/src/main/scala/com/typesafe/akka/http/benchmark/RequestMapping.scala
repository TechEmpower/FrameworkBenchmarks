package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.model.headers.Connection
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.handlers._

trait RequestMapping { _: PlaintextHandler with JsonHandler with DbHandler with QueriesHandler with FortunesHandler with UpdatesHandler =>
  def asRoute: Route =
    plainTextEndpoint ~ jsonEndpoint ~ dbEndpoint ~ queriesEndpoint ~ fortunesEndpoint ~ updatesEndpoint
}
