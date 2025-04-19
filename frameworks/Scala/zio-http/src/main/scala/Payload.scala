import zio.json.{DeriveJsonCodec, JsonCodec}

case class Payload(message: String)
object Payload {
  implicit val codec: JsonCodec[Payload] = DeriveJsonCodec.gen
}