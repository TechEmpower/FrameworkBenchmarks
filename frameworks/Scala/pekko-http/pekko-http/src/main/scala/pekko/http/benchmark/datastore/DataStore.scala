package pekko.http.benchmark.datastore

import pekko.http.benchmark.entity.{Fortune, World}

import scala.collection.immutable
import scala.concurrent.Future

trait DataStore {
  def findWorldById(id: Int): Future[Option[World]]

  def requireWorldById(id: Int): Future[World]

  def updateWorld(world: World): Future[Boolean]

  def getFortunes: Future[Seq[Fortune]]
}
