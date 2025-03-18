package com.example.starter

import com.example.starter.utils.PeriodicDateResolver
import com.example.starter.utils.block
import io.vertx.core.Vertx
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import kotlin.time.Duration.Companion.seconds
import org.apache.logging.log4j.kotlin.Logging

object App : Logging {
    private const val SERVER_NAME = "Vert.x-Web Benchmark"

    @JvmStatic
    fun main(args: Array<out String?>?) {
        val eventLoopPoolSize = System.getProperty("vertx.eventLoopPoolSize")?.toInt()
            ?: Runtime.getRuntime().availableProcessors()

        val vertx = Vertx.vertx(
            vertxOptionsOf(
                eventLoopPoolSize = eventLoopPoolSize,
                preferNativeTransport = true,
            )
        )
        vertx.exceptionHandler {
            logger.error(it) { "Vertx unexpected exception:" }
            vertx.close().block(5.seconds)
        }

        // Add the SIGINT handler
        Runtime.getRuntime().addShutdownHook(Thread {
            vertx.close().block(5.seconds)
        })

        // Initialize the periodic date resolver
        PeriodicDateResolver.init(vertx)

        // Check the type of test that is being run
        val hasDb = System.getProperty("tfb.hasDB")?.toBoolean() ?: false

        vertx.deployVerticle(
            { if (hasDb) PostgresVerticle() else BasicVerticle() },
            deploymentOptionsOf(
                instances = eventLoopPoolSize,
            )
        )

        logger.info("$SERVER_NAME started.")
    }
}