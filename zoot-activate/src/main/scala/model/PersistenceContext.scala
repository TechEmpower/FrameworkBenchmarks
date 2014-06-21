package model

import net.fwbrasil.activate.ActivateContext
import net.fwbrasil.activate.cache.CacheType
import net.fwbrasil.activate.storage.StorageFactory
import net.fwbrasil.activate.storage.relational.async.AsyncMySQLStorage
import net.fwbrasil.activate.storage.relational.idiom.mySqlDialect
import net.fwbrasil.activate.storage.relational.async.AsyncPostgreSQLStorage
import net.fwbrasil.activate.storage.relational.idiom.postgresqlDialect

object persistenceContext extends ActivateContext {

    val storage = StorageFactory.fromEnvVariables("benchmark") match {
        case storage: AsyncMySQLStorage =>
            new AsyncMySQLStorage {
                override val dialect: mySqlDialect = mySqlDialect(normalizeRandomNumberColumnName)
                lazy val objectFactory = storage.objectFactory
                override def poolConfiguration = storage.poolConfiguration
            }
        case storage: AsyncPostgreSQLStorage =>
            new AsyncPostgreSQLStorage {
                override val dialect: postgresqlDialect = postgresqlDialect(normalizeRandomNumberColumnName)
                lazy val objectFactory = storage.objectFactory
                override def poolConfiguration = storage.poolConfiguration
            }
        case other => other
    }

    private def normalizeRandomNumberColumnName(string: String) =
        string match {
            case "randomNumber" =>
                "randomnumber"
            case other =>
                other
        }

    override def liveCacheType = CacheType.weakReferences
}
