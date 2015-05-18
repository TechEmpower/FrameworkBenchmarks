package com.typesafe.akka.http.benchmark.datastore

import com.typesafe.akka.http.benchmark.entity.{Fortune, World}

import scala.concurrent.Future

trait DataStore {
  def findOne(id: Int): Future[World]

  def updateOne(id: Int, randomNumber: Int): Future[Boolean]

  def getFortunes: Future[List[Fortune]]
}
