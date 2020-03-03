package net.benchmark.akka.http.world

import io.circe.{Decoder, Encoder}

object Hello {

  implicit val decodeWorld: Decoder[Hello] =
    Decoder.forProduct1("message")(Hello.apply)

  implicit val encodeWorld: Encoder[Hello] =
    Encoder.forProduct1("message")(v => v.message)

}

case class Hello(message: String)
