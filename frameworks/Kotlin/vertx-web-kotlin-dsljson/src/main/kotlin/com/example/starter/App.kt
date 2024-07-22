package com.example.starter

import com.example.starter.utils.awaitClose
import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import kotlin.time.Duration.Companion.seconds
import org.apache.logging.log4j.kotlin.Logging

class App {
    companion object : Logging {
        private const val SERVER_NAME = "Vert.x-Web Benchmark"

        @JvmStatic
        fun main(args: Array<out String?>?) {
            val numCores = CpuCoreSensor.availableProcessors()
            val vertx = Vertx.vertx(
                vertxOptionsOf(
                    eventLoopPoolSize = numCores,
                    preferNativeTransport = true,
                )
            )

            vertx.exceptionHandler {
                logger.error(it) { "Vertx unexpected exception:" }
                vertx.awaitClose(5.seconds)
            }

            Runtime.getRuntime().addShutdownHook(Thread { vertx.awaitClose(5.seconds) })

            val hasDb = System.getProperty("tfb.hasDB")?.toBoolean() ?: false

            vertx.deployVerticle(
                { if (hasDb) { PgVerticle() } else { DefaultVerticle() } },
                deploymentOptionsOf(
                    instances = numCores
                )
            )

            logger.info("$SERVER_NAME started.")
        }
    }
}