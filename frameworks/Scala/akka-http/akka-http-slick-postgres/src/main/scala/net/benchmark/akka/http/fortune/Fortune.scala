package net.benchmark.akka.http.fortune
import io.circe.Codec, io.circe.generic.semiauto.deriveCodec

case class Fortune(id: Int, message: String)

object Fortune {

  implicit val fortuneCodec: Codec[Fortune] = deriveCodec

  def tupled = (this.apply _).tupled
}
