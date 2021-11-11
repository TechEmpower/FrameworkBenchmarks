package net.benchmark.akka.http.world

import io.circe.Codec, io.circe.generic.semiauto.deriveCodec

case class World(id: Int, randomNumber: Int)

object World {

  implicit val worldCodec: Codec[World] = deriveCodec

  def tupled = (this.apply _).tupled
}
