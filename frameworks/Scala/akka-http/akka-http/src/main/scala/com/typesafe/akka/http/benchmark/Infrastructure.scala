package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

trait Infrastructure {
  implicit def system: ActorSystem

  implicit def executionContext: ExecutionContext

  def appConfig: Config
}
