package net.benchmark.akka.http.world
import io.circe.{Decoder, Encoder}

object World {

  implicit val decodeWorld: Decoder[World] =
    Decoder.forProduct2("id", "randomNumber")(World.apply)

  implicit val encodeWorld: Encoder[World] =
    Encoder.forProduct2("id", "randomNumber")(v => (v.id, v.randomNumber))

  def tupled = (this.apply _).tupled
}

case class World(id: Int, randomNumber: Int)
