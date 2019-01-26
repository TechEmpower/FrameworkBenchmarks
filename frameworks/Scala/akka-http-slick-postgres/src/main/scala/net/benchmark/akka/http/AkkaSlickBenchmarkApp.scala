package net.benchmark.akka.http

import akka.actor.{ActorRef, ActorSystem, Terminated}
import akka.stream.ActorMaterializer
import com.monovore.decline.{CommandApp, Opts}
import com.typesafe.config.Config
import net.benchmark.akka.http.AkkaSlickBenchmarkApi.ApiMessages
import net.benchmark.akka.http.db._
import org.slf4j.{Logger, LoggerFactory}
import slick.basic.DatabaseConfig

import scala.concurrent.{ExecutionContext, Future}

object AkkaSlickBenchmarkApp
    extends CommandApp(
      name = "AkkaSlickBenchmarkApp",
      header = "Start the App.",
      main = {
        Opts.unit.map { _ =>
          AppFunctions.boot()
        }
      }
    )

object AppFunctions {
  private final val log: Logger = LoggerFactory.getLogger(this.getClass)

  def boot(): Unit = {
    implicit val system: ActorSystem = ActorSystem("AkkaSlickBenchmarkApp")
    implicit val mat: ActorMaterializer = ActorMaterializer()

    val config: Config = system.settings.config

    val dbConfig: DatabaseConfig[CustomPostgresProfile] = DatabaseConfiguration.getDefaultDatabaseConfiguration(config)
    val dbLoader: DatabaseRepositoryLoader = new DatabaseRepositoryLoaderModule(dbConfig)

    val api: ActorRef = system.actorOf(AkkaSlickBenchmarkApi.props(dbLoader, mat))
    api ! ApiMessages.StartApi

    val finish: Future[Terminated] = system.whenTerminated
    finish.onComplete(_ => log.info("akka-slick-benchmark app terminated!"))(
      ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newFixedThreadPool(1)))
  }

}
