package server

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import model.persistenceContext.transactional
import net.fwbrasil.zoot.core.mapper.JacksonStringMapper

object Main extends App {
    
    implicit val executionContext = {
        config("executionContext", "forkjoin") match {
            case "forkjoin" => ExecutionContext.global
            case "cached" => ExecutionContext.fromExecutor(Executors.newCachedThreadPool)
        }
    }
    implicit val mirror = scala.reflect.runtime.currentMirror
    implicit val mapper = new JacksonStringMapper

    transactional {}

    val port = config("port", "9000").toInt

    config("server", "finagle") match {
        case "spray" => new SprayServer(port).start
        case "finagle" => new FinagleServer(port).start
    }
    
    private def config(name: String, default: String) = 
        Option(System.getenv(name)).getOrElse(default)
}
