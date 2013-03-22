dataSource {
    pooled = true
    dbCreate = "update"
    url = "jdbc:mysql://localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true"
    driverClassName = "com.mysql.jdbc.Driver"
    dialect = org.hibernate.dialect.MySQL5InnoDBDialect
    username = "benchmarkdbuser"
    password = "benchmarkdbpass"
    properties {
        maxActive = 256
        maxIdle = 25
        minIdle = 5
        initialSize = 5
        minEvictableIdleTimeMillis = 60000
        timeBetweenEvictionRunsMillis = 60000
        maxWait = 10000
        validationQuery = "/* ping */"
    }
}
hibernate {
    // Purposely turning off query cache
    cache.use_second_level_cache = false
    cache.use_query_cache = false
    cache.region.factory_class = 'net.sf.ehcache.hibernate.EhCacheRegionFactory'
}
