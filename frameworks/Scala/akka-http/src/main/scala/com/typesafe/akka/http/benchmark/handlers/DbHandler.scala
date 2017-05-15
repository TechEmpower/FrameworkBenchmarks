package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.util.RandomGenerator

trait DbHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

  def dbEndpoint =
    get {
      path("db") {
        val id = nextRandomIntBetween1And10000
        complete(requireWorldById(id))
      }
    }
}