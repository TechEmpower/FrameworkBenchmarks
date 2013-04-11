package bench

import com.typesafe.config.{ ConfigFactory, Config }

object Server {
  val logger = org.clapper.avsl.Logger(Server.getClass)

  def main(args: Array[String]) {
    val config = ConfigFactory.load()
    DatabaseAccess.loadConfiguration(config)

    unfiltered.netty.Http(9000)
      .handler(Plans)
      .run { s =>
        logger.info("starting unfiltered app at localhost on port %s".format(s.port))
      }

    dispatch.Http.shutdown()
  }
}
