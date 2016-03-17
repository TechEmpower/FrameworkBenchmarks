package example

import colossus._
import colossus.core.{Initializer, Server, ServerRef, ServerSettings}
import colossus.service._
import Callback.Implicits._
import colossus.protocols.http._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._

object BenchmarkService {

  implicit object JsonBody extends HttpBodyEncoder[JValue] {
    val jsonHeader            = HttpHeader("Content-Type", "application/json")
    def encode(json: JValue)  = new HttpBody(compact(render(json)).getBytes("UTF-8"), Some(jsonHeader))
  }

  val json : JValue     = ("message" -> "Hello, World!")
  val plaintext         = HttpBody("Hello, World!")
  val serverHeader      = HttpHeader("Server", "Colossus")

  def start(port: Int)(implicit io: IOSystem) {

    val serverConfig = ServerSettings(
      port = port,
      maxConnections = 16384,
      tcpBacklogSize = Some(1024)
    )
    val serviceConfig = ServiceConfig(
      requestMetrics = false
    )

    Server.start("benchmark", serverConfig) { new Initializer(_) {

      val dateHeader = new DateHeader
      val headers = HttpHeaders(serverHeader, dateHeader)

      def onConnect = ctx => new Service[Http](serviceConfig, ctx){ 
        def handle = { 
          case req if (req.head.url == "/plaintext")  => req.ok(plaintext, headers)
          case req if (req.head.url == "/json")       => req.ok(json, headers)
        }
      }
    }}
  }

}


object Main extends App {

  implicit val io_system = IOSystem()

  BenchmarkService.start(9007)

}
