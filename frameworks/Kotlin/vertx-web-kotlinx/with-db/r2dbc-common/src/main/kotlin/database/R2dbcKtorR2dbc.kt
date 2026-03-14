package database

import io.r2dbc.pool.ConnectionPool
import io.r2dbc.pool.ConnectionPoolConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration
import io.r2dbc.postgresql.PostgresqlConnectionFactory
import io.r2dbc.postgresql.client.SSLMode
import io.r2dbc.spi.ConnectionFactory
import java.time.Duration

// Alternative configurations copied from the `ktor-r2dbc` portion. These configurations don't lead to better performance as tested in Continuous Benchmarking.

val connectionFactoryKtorR2dbc: ConnectionFactory = PostgresqlConnectionFactory(
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

fun connectionPoolConfigurationKtorR2dbc(size: Int) =
    ConnectionPoolConfiguration.builder(connectionFactoryKtorR2dbc)
        .initialSize(size)
        .maxSize(size)
        .maxIdleTime(Duration.ofSeconds(30))
        .maxAcquireTime(Duration.ofSeconds(5))
        .build()

fun connectionPoolKtorR2dbc(size: Int) =
    ConnectionPool(connectionPoolConfigurationKtorR2dbc(size))
