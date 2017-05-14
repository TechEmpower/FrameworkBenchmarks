package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.Materializer
import com.typesafe.akka.http.benchmark.datastore.MySqlDataStore
import com.typesafe.akka.http.benchmark.handlers._
import com.typesafe.akka.http.benchmark.util.RandomGenerator
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.fusesource.scalate.Binding
import org.fusesource.scalate.TemplateEngine

import scala.concurrent.ExecutionContext

class App extends Infrastructure with RandomGenerator with MySqlDataStore with PlaintextHandler with JsonHandler with DbHandler with QueriesHandler with FortunesHandler with UpdatesHandler with RequestMapping with BenchmarkBootstrap with Templating {
  lazy val templateEngine = new TemplateEngine()

  implicit lazy val system: ActorSystem = ActorSystem("akka-http-benchmark")
  lazy val executionContext: ExecutionContext = system.dispatcher
  lazy val materializer: Materializer = ActorMaterializer()
  lazy val appConfig: Config = ConfigFactory.load

  def layout(uri: String, attributes: Map[String, Any], extraBindings: Traversable[Binding]): String =
    templateEngine.layout(uri, attributes, extraBindings)
}

