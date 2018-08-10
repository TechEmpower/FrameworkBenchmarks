package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.util.RandomGenerator

trait DbHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import de.heikoseeberger.akkahttpjsoniterscala.JsoniterScalaSupport._

  def dbEndpoint: Route =
    get {
      path("db") {
        complete(requireWorldById(nextRandomIntBetween1And10000))
      }
    }
}