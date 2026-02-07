package com.example.starter

import com.example.starter.helpers.PeriodicResolver
import com.example.starter.helpers.Properties
import com.example.starter.utils.block
import io.vertx.core.Vertx
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.coAwait
import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.runBlocking
import org.apache.logging.log4j.kotlin.logger

private val LOGGER = logger("App")

fun main(): Unit = runBlocking {
    val vertx = Vertx.vertx(
        vertxOptionsOf(
            eventLoopPoolSize = Properties.EVENT_LOOP_POOL_SIZE,
            preferNativeTransport = true,
            disableTCCL = true,
            blockedThreadCheckInterval = 60_000,
        )
    )

    if (!vertx.isNativeTransportEnabled) {
        throw IllegalStateException(
            "Native transport not enabled; missing required dependencies",
            vertx.unavailableNativeTransportCause(),
        )
    }

    vertx.exceptionHandler {
        LOGGER.error("Vertx unexpected exception", it)
    }

    Runtime.getRuntime().addShutdownHook(
        Thread {
            vertx.close().block(5.seconds)
        }
    )

    PeriodicResolver.init(vertx)

    val options = deploymentOptionsOf(
        instances = Properties.EVENT_LOOP_POOL_SIZE,
    )

    LOGGER.info("Initializing Verticle: ${Properties.TYPE}")

    val deployment = when (Properties.TYPE) {
        "basic" -> vertx.deployVerticle(
            BasicVerticle::class.java,
            options
        )
        "postgres" -> vertx.deployVerticle(
            PostgresVerticle::class.java,
            options
        )
        "all" -> vertx.deployVerticle(
            ServerVerticle::class.java,
            options
        )
        else -> throw IllegalStateException("Unknown deployment type: ${Properties.TYPE}")
    }

    deployment.coAwait()

    LOGGER.info("${Properties.SERVER_NAME} started.")
}
