package com.typesafe.akka.http.benchmark.handlers

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.Infrastructure
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.entity.World
import com.typesafe.akka.http.benchmark.util.RandomGenerator

import scala.concurrent.Future
import scala.util.Try

trait UpdatesHandler { _: Infrastructure with DataStore with RandomGenerator =>
  import de.heikoseeberger.akkahttpjsoniterscala.JsoniterScalaSupport._

  def updatesEndpoint: Route =
    get {
      path("updates") {
        parameter("queries".?) { numQueries =>
          val realNumQueries = Try(numQueries.getOrElse("1").toInt).getOrElse(1).min(500).max(1)

          def mutateOne(id: Int): Future[World] =
            for {
              world <- requireWorldById(id)
              newWorld = world.copy(randomNumber = nextRandomIntBetween1And10000)
              _ <- updateWorld(newWorld) // ignore `wasUpdated`
            } yield newWorld

          complete {
            Future.traverse(Seq.fill(realNumQueries)(nextRandomIntBetween1And10000))(mutateOne)
          }
        }
      }
    }
}
