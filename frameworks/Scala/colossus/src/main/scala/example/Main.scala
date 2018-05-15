package example

import akka.actor.ActorSystem
import colossus.core._
import colossus.protocols.http._
import colossus.service._
import colossus.service.Callback.Implicits._
import colossus.service.GenRequestHandler.PartialHandler
import colossus.util.DataSize
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.concurrent.duration.Duration

case class Message(message: String)

object Main extends App {

  val serverConfig = ServerSettings(
    port = 9007,
    maxConnections = 16384,
    tcpBacklogSize = Some(1024))

  val serviceConfig = ServiceConfig(
    logErrors = false,
    requestMetrics = false,
    requestTimeout = Duration("1s"),
    requestBufferSize = 65536,
    maxRequestSize = DataSize(1024 * 1024))

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ioSystem: IOSystem = IOSystem()

  implicit val codec: JsonValueCodec[Message] = JsonCodecMaker.make[Message](CodecMakerConfig())

  implicit val messageEncoder: HttpBodyEncoder[Message] = new HttpBodyEncoder[Message] {
    override def encode(data: Message): HttpBody = new HttpBody(writeToArray(data))
    override def contentType: String = "application/json"
  }

  HttpServer.start("Colossus", serverConfig)(initContext => new Initializer(initContext) {
    override def onConnect: RequestHandlerFactory = serverContext => new RequestHandler(serverContext, serviceConfig) {
      override def handle: PartialHandler[Http] = {
        case req if req.head.url == "/plaintext" => req.ok("Hello, World!")
        case req if req.head.url == "/json" => req.ok(Message("Hello, World!"))
      }
    }
  })
}
