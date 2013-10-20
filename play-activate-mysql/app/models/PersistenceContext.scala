package models

import net.fwbrasil.activate.ActivateContext
import net.fwbrasil.activate.storage.relational.idiom.postgresqlDialect
import net.fwbrasil.activate.storage.relational.PooledJdbcRelationalStorage
import net.fwbrasil.activate.storage.relational.idiom.mySqlDialect
import play.api.Play
import net.fwbrasil.activate.OptimisticOfflineLocking

object persistenceContext extends ActivateContext {
	require(OptimisticOfflineLocking.isEnabled)
	require(OptimisticOfflineLocking.validateReads)
    
    private def config = Play.current.configuration
    
    val storage = new PooledJdbcRelationalStorage {
        val jdbcDriver = config.getString("db.default.driver").get
        val user = config.getString("db.default.user").get
        val password = config.getString("db.default.password").get
        val url = config.getString("db.default.url").get
        val dialect = mySqlDialect
        override val poolSize = 400
    }
    
    val indexWorldByLegacyId = memoryIndex[ActivateWorld].on(_.legacyId)
    val indexFortuneAll = memoryIndex[ActivateFortune].on(_ => 1)

}