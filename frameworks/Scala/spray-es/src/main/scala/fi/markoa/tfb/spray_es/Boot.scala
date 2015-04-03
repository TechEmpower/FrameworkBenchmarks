package fi.markoa.tfb.spray_es

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import spray.can.Http
import scala.concurrent.duration._

object Boot extends App {
  val logger = Logger(LoggerFactory.getLogger(Boot.getClass))

  implicit val system = ActorSystem("tfb-service")

  val service = system.actorOf(Props[BenchmarkServiceActor], "tfb-service")

  implicit val timeout = Timeout(5.seconds)
  IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = 8080)
}