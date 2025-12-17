package database

import io.r2dbc.pool.ConnectionPool
import io.r2dbc.pool.ConnectionPoolConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionFactory
import io.r2dbc.postgresql.client.SSLMode
import io.r2dbc.spi.ConnectionFactory
import java.time.Duration

// copied and adapted from the `ktor-r2dbc` portion

fun configurePostgresR2DBC(): ConnectionFactory {
    val cfo = PostgresqlConnectionConfiguration.builder()
        .host("tfb-database")
        .port(5432)
        .database("hello_world")
        .username("benchmarkdbuser")
        .password("benchmarkdbpass")
        //.loopResources { NioClientEventLoopResources(Runtime.getRuntime().availableProcessors()).cacheLoops() }
        .sslMode(SSLMode.DISABLE)
        .tcpKeepAlive(true)
        .tcpNoDelay(true)
        .build()

    val cf = PostgresqlConnectionFactory(cfo)

    val cp = ConnectionPoolConfiguration.builder(cf)
        .initialSize(512)
        .maxSize(512)
        .maxIdleTime(Duration.ofSeconds(30))
        .maxAcquireTime(Duration.ofSeconds(5))
        .validationQuery("SELECT 1")
        .build()

    return ConnectionPool(cp)
}
