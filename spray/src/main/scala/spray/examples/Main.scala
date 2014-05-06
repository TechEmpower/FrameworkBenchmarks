package spray.examples

import akka.actor._
import akka.io.IO
import spray.can.Http

object Main extends App {

  implicit val system = ActorSystem()

  // the handler actor replies to incoming HttpRequests
  val handler = system.actorOf(Props[BenchmarkService], name = "handler")

  val interface = "127.0.0.1"//system.settings.config.getString("app.interface")
  val port = 8080 //system.settings.config.getInt("app.port")
  IO(Http) ! Http.Bind(handler, interface, port)
}
