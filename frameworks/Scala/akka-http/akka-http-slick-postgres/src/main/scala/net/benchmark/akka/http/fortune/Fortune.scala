package net.benchmark.akka.http.fortune
import io.circe.{Decoder, Encoder}

object Fortune {

  implicit val decodeFortune: Decoder[Fortune] =
    Decoder.forProduct2("id", "message")(Fortune.apply)

  implicit val encodeFortune: Encoder[Fortune] =
    Encoder.forProduct2("id", "message")(v => (v.id, v.message))

  def tupled = (this.apply _).tupled
}

case class Fortune(id: Int, message: String)
