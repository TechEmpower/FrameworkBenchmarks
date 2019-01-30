package net.benchmark.akka.http

import com.typesafe.config.{Config, ConfigFactory}

object AppConfig {

  val config: Config = ConfigFactory.load()

}
