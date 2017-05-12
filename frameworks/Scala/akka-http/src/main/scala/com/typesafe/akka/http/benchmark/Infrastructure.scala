package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait Infrastructure {
  implicit def system: ActorSystem
  implicit def executionContext: ExecutionContext
  implicit def materializer: Materializer
  def appConfig: Config
}
