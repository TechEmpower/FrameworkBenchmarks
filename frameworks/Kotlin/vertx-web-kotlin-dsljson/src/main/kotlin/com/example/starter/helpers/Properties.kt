package com.example.starter.helpers

import io.netty.util.internal.SystemPropertyUtil
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.core.tracing.TracingPolicy
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.pgclient.pgConnectOptionsOf

object Properties {

    /**
     * The server name (used in headers and logging).
     * Default: `Vert.x-Web`
     */
    val SERVER_NAME: String = SystemPropertyUtil.get("tfb.serverName", "Vert.x-Web")

    /**
     * Type of verticle to deploy.
     * Default: all ([com.example.starter.ServerVerticle])
     */
    val TYPE: String = SystemPropertyUtil.get("tfb.type", "all")

    /**
     * Number of event loop threads.
     * Default: [io.vertx.core.impl.cpu.CpuCoreSensor.availableProcessors]
     */
    val EVENT_LOOP_POOL_SIZE: Int = SystemPropertyUtil.getInt("tfb.eventLoopPoolSize", CpuCoreSensor.availableProcessors())

    /**
     * Whether metrics are enabled.
     * Default: Reverse of `vertx.disableMetrics` (defaults to true)
     */
    val METRICS_ENABLED: Boolean = SystemPropertyUtil.getBoolean("vertx.disableMetrics", true).not()

    /**
     * Port the HTTP server listens on.
     * Default: 8080 (tfb.http.port)
     */
    val HTTP_PORT: Int = SystemPropertyUtil.getInt("tfb.http.port", 8080)

    /**
     * Size of TCP send buffer for HTTP connections, in bytes.
     * Default: 32768 (tfb.http.sendBufferSize)
     */
    val HTTP_SEND_BUFFER_SIZE: Int = SystemPropertyUtil.getInt("tfb.http.sendBufferSize", 256 * 1024)

    /**
     * Size of TCP receive buffer for HTTP connections, in bytes.
     * Default: 32768 (tfb.http.receiveBufferSize)
     */
    val HTTP_RECEIVE_BUFFER_SIZE: Int = SystemPropertyUtil.getInt("tfb.http.receiveBufferSize", 256 * 1024)

    /**
     * Enables TCP Fast Open on the HTTP server.
     * Default: true (tfb.http.tcpFastOpen)
     */
    val HTTP_TCP_FASTOPEN: Boolean = SystemPropertyUtil.getBoolean("tfb.http.tcpFastOpen", false)

    /**
     * Enables TCP_NODELAY (disables Nagle) on HTTP connections.
     * Default: true (tfb.http.tcpNoDelay)
     */
    val HTTP_TCP_NODELAY: Boolean = SystemPropertyUtil.getBoolean("tfb.http.tcpNoDelay", false)

    /**
     * Idle timeout for HTTP connections in seconds.
     * 0 disables idle timeout.
     * Default: 0 (tfb.http.idleTimeout)
     */
    val HTTP_IDLE_TIMEOUT: Int = SystemPropertyUtil.getInt("tfb.http.idleTimeout", 0)

    /**
     * Enables SO_REUSEADDR on the HTTP server socket.
     * Default: true (tfb.http.reuseAddress)
     */
    val HTTP_REUSE_ADDRESS: Boolean = SystemPropertyUtil.getBoolean("tfb.http.reuseAddress", true)

    /**
     * Enables SO_REUSEPORT on the HTTP server socket.
     * Default: true (tfb.http.reusePort)
     */
    val HTTP_REUSE_PORT: Boolean = SystemPropertyUtil.getBoolean("tfb.http.reusePort", true)

    /**
     * Size of the TCP accept backlog for the HTTP server.
     * Default: 8192 (tfb.http.acceptBacklog)
     */
    val HTTP_ACCEPT_BACKLOG: Int = SystemPropertyUtil.getInt("tfb.http.acceptBacklog", 8192)

    /**
     * PostgreSQL username used for connections.
     * Default: benchmarkdbuser (tfb.pg.user)
     */
    val PG_USER: String = SystemPropertyUtil.get("tfb.pg.user", "benchmarkdbuser")

    /**
     * PostgreSQL password used for connections.
     * Default: benchmarkdbpass (tfb.pg.password)
     */
    val PG_PASSWORD: String = SystemPropertyUtil.get("tfb.pg.password", "benchmarkdbpass")

    /**
     * PostgreSQL host used for connections.
     * Default: tfb.pgHostOverride system property, otherwise "tfb-database".
     * Property: tfb.pg.host
     */
    val PG_HOST: String = SystemPropertyUtil.get("tfb.pg.host", System.getProperty("tfb.pgHostOverride") ?: "tfb-database")

    /**
     * PostgreSQL port used for connections.
     * Default: 5432 (tfb.pg.port)
     */
    val PG_PORT: Int = SystemPropertyUtil.getInt("tfb.pg.port", 5432)

    /**
     * PostgreSQL database name used for connections.
     * Default: hello_world (tfb.pg.database)
     */
    val PG_DATABASE: String = SystemPropertyUtil.get("tfb.pg.database", "hello_world")

    /**
     * Enables prepared statement caching on the PostgreSQL client.
     * Default: true (tfb.pg.cachePreparedStatements)
     */
    val PG_CACHE_PREPARED_STATEMENTS: Boolean = SystemPropertyUtil.getBoolean("tfb.pg.cachePreparedStatements", true)

    /**
     * Maximum size of the prepared statement cache.
     * Default: 1024 (tfb.pg.preparedStatementCacheMaxSize)
     */
    val PG_PREPARED_STATEMENT_CACHE_MAX_SIZE: Int =
        SystemPropertyUtil.getInt("tfb.pg.preparedStatementCacheMaxSize", 1024)

    /**
     * Max number of in-flight pipelined requests per connection.
     * Default: 1024 (tfb.pg.pipeliningLimit)
     */
    val PG_PIPELINING_LIMIT: Int = SystemPropertyUtil.getInt("tfb.pg.pipeliningLimit", 1024)

    val HTTP by lazy {
        httpServerOptionsOf(
            port = HTTP_PORT,
            compressionSupported = false,
            tracingPolicy = TracingPolicy.IGNORE,
            http2ClearTextEnabled = false,
            strictThreadMode = true,
        )
    }

    val PG_CONNECT by lazy {
        pgConnectOptionsOf(
            user = PG_USER,
            password = PG_PASSWORD,
            host = PG_HOST,
            port = PG_PORT,
            database = PG_DATABASE,
            cachePreparedStatements = PG_CACHE_PREPARED_STATEMENTS,
            preparedStatementCacheMaxSize = PG_PREPARED_STATEMENT_CACHE_MAX_SIZE,
            tracingPolicy = TracingPolicy.IGNORE,
            pipeliningLimit = PG_PIPELINING_LIMIT
        )
    }
}
