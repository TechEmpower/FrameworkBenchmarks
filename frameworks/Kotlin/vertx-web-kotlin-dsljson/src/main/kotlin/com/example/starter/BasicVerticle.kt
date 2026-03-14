package com.example.starter

import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.helpers.Properties
import com.example.starter.utils.isConnectionReset
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.coAwait
import org.apache.logging.log4j.kotlin.Logging

class BasicVerticle : CoroutineVerticle() {
    override suspend fun start() {
        val defaultHandler = DefaultHandler()
        val messageHandler = MessageHandler()

        val server = vertx
            .createHttpServer(Properties.HTTP)
            .requestHandler {
                val path = it.path()
                val code = when (path.length) {
                    10 -> if (path == PLAINTEXT_PATH) 1 else 0
                    5  -> if (path == JSON_PATH)      2 else 0
                    else -> 0
                }
                when (code) {
                    1 -> defaultHandler.plaintext(it)
                    2 -> messageHandler.readDefaultMessage(it)
                    else -> it.response().setStatusCode(404).end()
                }
            }
            .exceptionHandler {
                if (!it.isConnectionReset()) {
                    logger.error("Exception in HttpServer", it)
                }
            }
            .listen()
            .coAwait()

        logger.info("HTTP server started on port ${server.actualPort()}")
    }

    private companion object : Logging {
        private const val PLAINTEXT_PATH = "/plaintext"
        private const val JSON_PATH = "/json"
    }
}
