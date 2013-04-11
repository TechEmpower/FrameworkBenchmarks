package bench

import scala.collection.immutable.Map
import scala.util.Try
import scala.slick.driver.SQLServerDriver.simple._
import java.util.Map.Entry
import com.typesafe.config._
import com.jolbox.bonecp.{ BoneCP, BoneCPConfig, BoneCPDataSource }

object DatabaseAccess {
	var configs: Map[String, BoneCPConfig] = Map[String, BoneCPConfig]()
	var dataSources: Map[String, BoneCPDataSource] = Map[String, BoneCPDataSource]()
	var databases: Map[String, Database] = dataSources.map { case(key, value) => (key, Database.forDataSource(value)) }
  var maxThreads: Int = 16 // shoehorning this in

  /** Loads the configuration given.  Usually loaded from application.conf in class path.
   *
   * @param config the configuration to use
   */
  def loadConfiguration(config: Config) {
    val keys = Seq(config.getObject("db").toConfig.entrySet.toArray: _*)
    val entries = keys.map("db." + _.asInstanceOf[Entry[String, Object]].getKey)
    val driverStrings = entries.filter(_.contains(".driver")).distinct
    val urls = entries.filter(_.contains(".url"))

    /* Load class drivers */
    for (driverString <- driverStrings) Class.forName(config.getString(driverString))

    /* Load config */
    DatabaseAccess.configs = (for (url <- urls) yield {
      /* Keys should be in the format db.key.url */
      val key = url.split('.').init.mkString(".") // db.key.url becomes db.key
      val boneCPConfig = new BoneCPConfig()
      boneCPConfig.setJdbcUrl(config.getString(url))
      boneCPConfig.setMinConnectionsPerPartition(config.getInt(key + ".minConnections"))
      boneCPConfig.setMaxConnectionsPerPartition(config.getInt(key + ".maxConnections"))
      boneCPConfig.setUsername(config.getString(key + ".user"))
      boneCPConfig.setPassword(config.getString(key + ".password"))
      boneCPConfig.setPartitionCount(2)
      (key, boneCPConfig)
    }).toMap
    DatabaseAccess.dataSources = DatabaseAccess.configs.map { case(key, value) => (key, new BoneCPDataSource(value)) }
    databases = dataSources.map { case(key, value) => (key, Database.forDataSource(value)) }
    maxThreads = Try { config.getString("unfiltered.maxThreads").toInt }.getOrElse(16) * 2
  }
}
