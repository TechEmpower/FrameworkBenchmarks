package net.benchmark.akka.http
import akka.actor.{ActorRef, ActorSystem, Terminated}
import akka.stream.ActorMaterializer
import com.typesafe.config.Config
import net.benchmark.akka.http.AkkaSlickBenchmarkApi.ApiMessages
import net.benchmark.akka.http.db.{
  CustomPostgresProfile,
  DatabaseConfiguration,
  DatabaseRepositoryLoader,
  DatabaseRepositoryLoaderModule
}
import org.slf4j.{Logger, LoggerFactory}
import slick.basic.DatabaseConfig

import scala.concurrent.{ExecutionContext, Future}

object Main {

  private final val log: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
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
