package com.typesafe.akka.http.benchmark.datastore

import scala.collection.immutable
import com.typesafe.akka.http.benchmark.entity.{ Fortune, World }

import scala.concurrent.Future

trait DataStore {
  def findWorldById(id: Int): Future[Option[World]]
  def requireWorldById(id: Int): Future[World]
  def updateWorld(world: World): Future[Boolean]

  def getFortunes: Future[immutable.Seq[Fortune]]
}
