package net.benchmark.akka.http.db

import com.typesafe.config.Config
import slick.basic.DatabaseConfig

object DatabaseConfiguration {

  /**
    * Return a slick database configuration which is defined in the given config
    * at the provided path.
    *
    * @param path   A path in the configuration at which the database is defined.
    * @param config A configuration.
    * @return A slick database configuration.
    */
  def getDatabaseConfiguration(path: String)(config: Config): DatabaseConfig[CustomPostgresProfile] = {
    DatabaseConfig.forConfig[CustomPostgresProfile](path, config)
  }

  // Load the database configuration from the default path.
  val getDefaultDatabaseConfiguration: Config => DatabaseConfig[CustomPostgresProfile] = getDatabaseConfiguration(
    "akka-http-slick-postgres.database")

}
