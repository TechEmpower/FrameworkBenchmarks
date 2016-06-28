package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.model.headers.Connection
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.handlers._

class RequestMapping(components: {
  val plaintextHandler: PlaintextHandler
  val jsonHandler: JsonHandler
  val dbHandler: DbHandler
  val queriesHandler: QueriesHandler
  val fortunesHandler: FortunesHandler
  val updatesHandler: UpdatesHandler
}) {
  val plaintext = components.plaintextHandler.endpoint
  val json = components.jsonHandler.endpoint
  val db = components.dbHandler.endpoint
  val queries = components.queriesHandler.endpoint
  val fortunes = components.fortunesHandler.endpoint
  val updates = components.updatesHandler.endpoint

  def asRoute: Route = {
    respondWithHeader(Connection("Keep-Alive")) {
       plaintext ~ json ~ db ~ queries ~ fortunes ~ updates
    }
  }
}
