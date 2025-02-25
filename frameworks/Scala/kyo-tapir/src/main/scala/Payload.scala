import sttp.tapir.Schema
import zio.json.*

case class Payload(message: String)
object Payload {
  given JsonCodec[Payload] = DeriveJsonCodec.gen
  given Schema[Payload] = Schema.derived
}