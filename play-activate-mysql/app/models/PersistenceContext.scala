package models

import net.fwbrasil.activate.ActivateContext
import net.fwbrasil.activate.storage.relational.idiom.postgresqlDialect
import net.fwbrasil.activate.storage.relational.PooledJdbcRelationalStorage
import net.fwbrasil.activate.storage.relational.idiom.mySqlDialect
import play.api.Play

object persistenceContext extends ActivateContext {
    
    private def config = Play.current.configuration
    
    val storage = new PooledJdbcRelationalStorage {
        val jdbcDriver = config.getString("db.default.driver").get
        val user = config.getString("db.default.user").get
        val password = config.getString("db.default.password").get
        val url = config.getString("db.default.url").get
        val dialect = mySqlDialect
    }
    
    val indexWorldByLegacyId = memoryIndex[ActivateWorld].on(_.legacyId)

}