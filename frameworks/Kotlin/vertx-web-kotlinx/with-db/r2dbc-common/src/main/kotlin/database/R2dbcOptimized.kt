package database

import io.r2dbc.pool.ConnectionPool
import io.r2dbc.pool.ConnectionPoolConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionFactory
import io.r2dbc.postgresql.client.SSLMode
import io.r2dbc.spi.ConnectionFactory
import java.time.Duration

// Optimized configuration with TCP settings and connection validation
val connectionFactoryOptimized: ConnectionFactory = PostgresqlConnectionFactory(
    PostgresqlConnectionConfiguration.builder()
        .host(HOST)
        .port(5432)
        .database(DATABASE)
        .username(USER)
        .password(PASSWORD)
        .sslMode(SSLMode.DISABLE)
        .tcpKeepAlive(true)
        .tcpNoDelay(true)
        .build()
)

fun connectionPoolConfigurationOptimized(size: Int) =
    ConnectionPoolConfiguration.builder(connectionFactoryOptimized)
        .initialSize(size)
        .maxSize(size)
        .maxIdleTime(Duration.ofSeconds(30))
        .maxAcquireTime(Duration.ofSeconds(5))
        .validationQuery("SELECT 1")
        .build()

fun connectionPoolOptimized(size: Int) =
    ConnectionPool(connectionPoolConfigurationOptimized(size))
