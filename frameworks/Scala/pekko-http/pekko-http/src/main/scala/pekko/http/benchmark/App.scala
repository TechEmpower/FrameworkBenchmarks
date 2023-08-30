package pekko.http.benchmark

import org.apache.pekko.actor.ActorSystem
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.fusesource.scalate.TemplateEngine
import pekko.http.benchmark.datastore.MySqlDataStore
import pekko.http.benchmark.handlers.{DbHandler, FortunesHandler, JsonHandler, QueriesHandler, UpdatesHandler}
import pekko.http.benchmark.util.RandomGenerator

import scala.concurrent.ExecutionContext

class App extends Infrastructure with RandomGenerator with MySqlDataStore with JsonHandler with DbHandler
  with QueriesHandler with FortunesHandler with UpdatesHandler with RequestMapping with BenchmarkBootstrap with Templating {

  val templateEngine = new TemplateEngine()
  implicit val system: ActorSystem = ActorSystem("pekko-http-benchmark")
  val executionContext: ExecutionContext = system.dispatcher
  val appConfig: Config = ConfigFactory.load
}

