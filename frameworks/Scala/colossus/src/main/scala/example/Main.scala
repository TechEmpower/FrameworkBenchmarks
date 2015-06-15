package example

import java.util.Date
import java.text.SimpleDateFormat

import akka.actor.{Actor, ActorRef, Props}
import akka.util.ByteString
import scala.concurrent.duration._
import colossus.IOSystem
import colossus.core.{ServerRef, ServerSettings}
import colossus.service._
import colossus.protocols.http._

import net.liftweb.json._
import JsonDSL._

object BenchmarkService {

  class Timestamp(server: ServerRef) extends Actor {
        
    val sdf = new SimpleDateFormat("EEE, MMM d yyyy HH:MM:ss z")
    case object Tick
    import context.dispatcher

    override def preStart() {
      self ! Tick
    }

    def receive = {
      case Tick => {
        server.delegatorBroadcast(sdf.format(new Date()))
        context.system.scheduler.scheduleOnce(1.second, self, Tick)
      }
    }
  }
  val response          = ByteString("Hello, World!")
  val plaintextHeader   = ("Content-Type", "text/plain")
  val jsonHeader        = ("Content-Type", "application/json")
  val serverHeader      = ("Server", "Colossus")


  def start(port: Int)(implicit io: IOSystem) {

    val serverConfig = ServerSettings(
      port = port,
      maxConnections = 16384,
      tcpBacklogSize = Some(1024)
    )
    val serviceConfig = ServiceConfig(
      name = "/sample",
      requestTimeout = Duration.Inf,
      requestMetrics = false
    )

    val server = Service.serve[Http](serverConfig, serviceConfig) { context =>

      ///the ??? is filled in almost immediately by the actor
      var dateHeader = ("Date", "???")

      context.receive {
        case ts: String => dateHeader = ("Date", ts)
      }
      
      context.handle { connection =>
        connection.become{ case request =>
          if (request.head.url == "/plaintext") {
            val res = HttpResponse(
              version  = HttpVersion.`1.1`,
              code    = HttpCodes.OK,
              data    = response,
              headers = Vector(plaintextHeader, serverHeader, dateHeader)
            )
            Callback.successful(res)
          } else if (request.head.url == "/json") {
            val json = ("message" -> "Hello, World!")
            val res = HttpResponse(
              version  = HttpVersion.`1.1`,
              code    = HttpCodes.OK,
              data    = compact(render(json)),
              headers = Vector(jsonHeader, serverHeader, dateHeader)
            )
            Callback.successful(res)
          } else {
            Callback.successful(request.notFound("invalid path"))
          }
        }
      }
    }

    val timestamp = io.actorSystem.actorOf(Props(classOf[Timestamp], server))
  }

}


object Main extends App {

  implicit val io_system = IOSystem()

  BenchmarkService.start(9007)

}
