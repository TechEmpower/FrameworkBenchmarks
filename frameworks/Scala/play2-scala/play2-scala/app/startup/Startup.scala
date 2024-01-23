package startup

import com.typesafe.config.Config
import jakarta.inject._
import play.api.Logger

@Singleton
class Startup @Inject()(config: Config) {

  private val logger = Logger(classOf[Startup])

  logger.info("System properties")
  logger.info("-----------------")
  logger.info("physical_cpu_count: " + System.getProperty("physical_cpu_count"))
  logger.info("thread_count: " + System.getProperty("thread_count"))
  logger.info("")
  logger.info("Configuration")
  logger.info("-------------")
  logger.info("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max: " + config.getInt("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max"))

}