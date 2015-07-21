package com.typesafe.akka.http.benchmark

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.typesafe.akka.http.benchmark.datastore.{DataStore, MySqlDataStore}
import com.typesafe.akka.http.benchmark.handlers._
import com.typesafe.akka.http.benchmark.util.RandomGenerator
import com.typesafe.config.{Config, ConfigFactory}
import org.fusesource.scalate.TemplateEngine

import scala.concurrent.ExecutionContext

trait Components {
  implicit val system = ActorSystem("akka-http-benchmark")
  implicit val executionContext: ExecutionContext = system.dispatcher
  lazy val config: Config = ConfigFactory.load
  lazy val randomGenerator = new RandomGenerator(this)
  lazy val dataStore: DataStore = new MySqlDataStore(this)
  lazy val plaintextHandler = new PlaintextHandler(this)
  lazy val jsonHandler = new JsonHandler(this)
  lazy val dbHandler = new DbHandler(this)
  lazy val queriesHandler = new QueriesHandler(this)
  lazy val fortunesHandler = new FortunesHandler(this)
  lazy val updatesHandler = new UpdatesHandler(this)
  lazy val route: Route = new RequestMapping(this).asRoute
  lazy val bootstrap: Bootstrap = new BenchmarkBootstrap(this)
  lazy val templateEngine: TemplateEngine = new TemplateEngine
}

object Components extends Components