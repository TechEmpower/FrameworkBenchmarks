import zio.json.{DeriveJsonCodec, JsonCodec}

case class Payload(message: String)
object Payload {
  given JsonCodec[Payload] = DeriveJsonCodec.gen
}
