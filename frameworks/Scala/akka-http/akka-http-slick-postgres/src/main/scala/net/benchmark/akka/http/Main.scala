package net.benchmark.akka.http
import akka.actor.{ActorRef, ActorSystem, Terminated}
import com.typesafe.config.Config
import net.benchmark.akka.http.ApiSupervisor.ApiMessages
import net.benchmark.akka.http.db.{DatabaseConfiguration, DatabaseRepositoryLoader, DatabaseRepositoryLoaderModule}
import net.benchmark.akka.http.util.SameThreadDirectExecutor
import org.slf4j.{Logger, LoggerFactory}
import slick.basic.DatabaseConfig
import slick.jdbc.PostgresProfile

import scala.concurrent.Future

object Main {

  private final val log: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("AkkaSlickBenchmarkApp")

    val pus = Runtime.getRuntime.availableProcessors()
    val pusMessage = s"Runtime.getRuntime.availableProcessors says ${pus.toString}"
    log.info(pusMessage)
    println(pusMessage)

    val config: Config = system.settings.config

    val dbConfig: DatabaseConfig[PostgresProfile] = DatabaseConfiguration.getDefaultDatabaseConfiguration(config)
    val dbLoader: DatabaseRepositoryLoader = new DatabaseRepositoryLoaderModule(dbConfig)

    val api: ActorRef = system.actorOf(ApiSupervisor.props(dbLoader))

    api ! ApiMessages.StartApi

    val finish: Future[Terminated] = system.whenTerminated
    finish.onComplete(_ => log.info("akka-http-slick-postgres app terminated!"))(
      SameThreadDirectExecutor.executionContext())
  }

}
