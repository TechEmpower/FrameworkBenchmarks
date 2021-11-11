package net.benchmark.akka.http.world

import io.circe.Codec, io.circe.generic.semiauto.deriveCodec

case class Hello(message: String)

object Hello {

  implicit val helloCodec: Codec[Hello] = deriveCodec

}
