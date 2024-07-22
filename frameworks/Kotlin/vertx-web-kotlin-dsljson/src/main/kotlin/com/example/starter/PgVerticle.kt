package com.example.starter

import com.example.starter.dagger.DaggerAppComponent
import io.vertx.core.AbstractVerticle
import io.vertx.core.Promise
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.pgclient.PgConnection
import java.util.concurrent.TimeUnit
import org.apache.logging.log4j.kotlin.Logging

class PgVerticle : AbstractVerticle() {
    override fun start(startPromise: Promise<Void>) {
        PgConnection.connect(vertx, PG_CONNECT_OPTIONS)
            .onSuccess { conn ->
                val component = DaggerAppComponent.builder()
                    .vertx(vertx)
                    .pgConnection(conn)
                    .build()

                val server = component.httpServer()

                server
                    .listen()
                    .onSuccess {
                        logger.info { "HTTP server started on port 8888" }
                        startPromise.complete()
                    }
                    .onFailure {
                        logger.error(it.cause) { "Failed to start" }
                        startPromise.fail(it.cause)
                    }
            }
            .onFailure {
                logger.error(it.cause) { "Failed to start" }
                startPromise.fail(it.cause)
            }
    }

    companion object : Logging {
        private const val PIPELINING_LIMIT = 100_000
        private const val RECV_BUFFER_SIZE = 256 * 1024 // 256 KB
        private const val SEND_BUFFER_SIZE = 256 * 1024 // 256 KB

        private val PG_CONNECT_OPTIONS = pgConnectOptionsOf(
            user = "benchmarkdbuser",
            password = "benchmarkdbpass",
            host = "tfb-database",
            port = 5432,
            database = "hello_world",
            cachePreparedStatements = true,
            preparedStatementCacheMaxSize = 256,
            preparedStatementCacheSqlLimit = 2048,
            tcpKeepAlive = true,
            tcpNoDelay = true,
            tcpFastOpen = true,
            tcpQuickAck = true,
            idleTimeout = 5,
            idleTimeoutUnit = TimeUnit.MINUTES,
            pipeliningLimit = PIPELINING_LIMIT,
            receiveBufferSize = RECV_BUFFER_SIZE,
            sendBufferSize = SEND_BUFFER_SIZE,
        )
    }
}