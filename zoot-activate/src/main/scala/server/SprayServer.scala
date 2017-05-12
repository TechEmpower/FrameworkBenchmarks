package server

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe.Mirror

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.io.IO
import akka.util.Timeout
import net.fwbrasil.zoot.core.Server
import net.fwbrasil.zoot.core.mapper.StringMapper
import net.fwbrasil.zoot.spray.{SprayServer => ZootSprayServer}
import service.BenchmarkApi
import service.BenchmarkService
import spray.can.Http

class SprayServer(port: Int)(implicit val executionContext: ExecutionContext, mirror: Mirror, mapper: StringMapper) {

    private implicit val system = ActorSystem("BenchmarkActorSystem")
    private implicit val timeout = Timeout(1000)

    import system.dispatcher

    val server = Server[BenchmarkApi](new BenchmarkService)

    val handler = system.actorOf(Props(new ZootSprayServer(server)))

    def start = IO(Http) ! Http.Bind(handler, interface = "localhost", port = port)
}