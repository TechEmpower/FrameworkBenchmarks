package com.typesafe.akka.http.benchmark.entity

import spray.json.DefaultJsonProtocol
import spray.json.RootJsonFormat

case class World(id: Int, randomNumber: Int)
object World {
  import DefaultJsonProtocol._
  implicit val worldFormat: RootJsonFormat[World] = jsonFormat(World.apply, "id", "randomNumber")
}
