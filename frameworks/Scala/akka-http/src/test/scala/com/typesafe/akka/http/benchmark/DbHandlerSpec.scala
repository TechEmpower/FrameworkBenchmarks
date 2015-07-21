package com.typesafe.akka.http.benchmark

import akka.http.scaladsl.server.RequestContext
import com.typesafe.akka.http.benchmark.datastore.DataStore
import com.typesafe.akka.http.benchmark.entity.{Fortune, World}
import com.typesafe.akka.http.benchmark.util.RandomGenerator
import org.scalatest._

import scala.concurrent.Future

class DbHandlerSpec extends FlatSpec with Matchers {
  val components = new Components {
    self =>

    import self.system.dispatcher

    override lazy val randomGenerator: RandomGenerator = new RandomGenerator(self) {
      override def next: Int = 1
    }
    override lazy val dataStore: DataStore = new DataStore {
      override def findOne(id: Int): Future[World] = {
        Future(World(1, 1))
      }

      override def updateOne(id: Int, randomNumber: Int): Future[Boolean] = Future(true)

      override def getFortunes: Future[List[Fortune]] = Future(List(Fortune(0, "Additional fortune added at request time.")))
    }
  }
  "A DbHandler" should "get random record" in {
  }
}
