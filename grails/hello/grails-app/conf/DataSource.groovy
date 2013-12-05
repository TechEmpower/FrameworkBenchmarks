import org.springframework.beans.factory.config.PropertiesFactoryBean
import org.springframework.core.io.support.ResourceArrayPropertyEditor

dataSource {
    pooled = true
    dbCreate = "update"
    url = "jdbc:mysql://localhost:3306/hello_world"
    driverClassName = "com.mysql.jdbc.Driver"
    dialect = org.hibernate.dialect.MySQL5InnoDBDialect
    username = "benchmarkdbuser"
    password = "benchmarkdbpass"
    properties {
        fairQueue = false
        maxActive = 512
        maxIdle = 25
        minIdle = 5
        initialSize = 5
        minEvictableIdleTimeMillis = 60000
        timeBetweenEvictionRunsMillis = 60000
        maxWait = 10000
        maxAge = 1800 * 1000
        numTestsPerEvictionRun=3
        testOnBorrow=false
        testWhileIdle=true
        testOnReturn=false
        validationQuery="/* ping */"
        validationInterval=15000
        jdbcInterceptors="ConnectionState;StatementCache"
        defaultTransactionIsolation = java.sql.Connection.TRANSACTION_READ_UNCOMMITTED
        dbProperties = this.loadProperties("classpath:mysql-connection.properties")
    }
}

hibernate {
    // Purposely turning off query cache
    cache.use_second_level_cache = false
    cache.use_query_cache = false
    cache.region.factory_class = 'net.sf.ehcache.hibernate.EhCacheRegionFactory'
    default_batch_fetch_size=256
    jdbc.fetch_size=256
    jdbc.batch_size=500
}

static Properties loadProperties(String path) {
    PropertiesFactoryBean pfb=new PropertiesFactoryBean()
    pfb.setIgnoreResourceNotFound(true)
    def converter=new ResourceArrayPropertyEditor()
    converter.setAsText(path)
    pfb.setLocations(converter.getValue())
    pfb.afterPropertiesSet()
    return pfb.object
}

