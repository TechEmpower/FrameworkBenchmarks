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
  logger.info("db_pool_size: " + System.getProperty("db_pool_size"))
  logger.info("")
  logger.info("Configuration")
  logger.info("-------------")
  logger.info("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max: " + config.getInt("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max"))
  logger.info("slick.dbs.default.db.numThreads: " + config.getInt("slick.dbs.default.db.numThreads"));

}