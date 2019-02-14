package net.benchmark.akka.http

import com.typesafe.config.{Config, ConfigFactory}

object AppConfig {

  val config: Config = ConfigFactory.load()

  object Queries {
    val routeDispatcherConfigPath: String =
      config.getString("akka-http-slick-postgres.queries.route-dispatcher-config-path")

  }

  object Updates {
    val routeDispatcherConfigPath: String =
      config.getString("akka-http-slick-postgres.updates.route-dispatcher-config-path")

  }

  object Db {
    val routeDispatcherConfigPath: String = config.getString("akka-http-slick-postgres.db.route-dispatcher-config-path")
  }

  object Fortunes {
    val routeDispatcherConfigPath: String =
      config.getString("akka-http-slick-postgres.fortunes.route-dispatcher-config-path")
  }

}
