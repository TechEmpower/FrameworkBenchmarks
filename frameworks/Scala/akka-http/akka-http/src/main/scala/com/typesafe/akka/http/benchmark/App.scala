package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import com.typesafe.akka.http.benchmark.datastore.MySqlDataStore
import com.typesafe.akka.http.benchmark.handlers._
import com.typesafe.akka.http.benchmark.util.RandomGenerator
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.fusesource.scalate.TemplateEngine

import scala.concurrent.ExecutionContext

class App extends Infrastructure with RandomGenerator with MySqlDataStore with PlaintextHandler with JsonHandler with DbHandler
  with QueriesHandler with FortunesHandler with UpdatesHandler with RequestMapping with BenchmarkBootstrap with Templating {

  val templateEngine = new TemplateEngine()
  implicit val system: ActorSystem = ActorSystem("akka-http-benchmark")
  val executionContext: ExecutionContext = system.dispatcher
  val appConfig: Config = ConfigFactory.load
}

