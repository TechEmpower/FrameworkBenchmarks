package com.example.starter

import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.utils.isConnectionReset
import io.vertx.core.AbstractVerticle
import io.vertx.core.Promise
import io.vertx.ext.web.Router
import io.vertx.kotlin.core.http.httpServerOptionsOf
import org.apache.logging.log4j.kotlin.Logging

class DefaultVerticle : AbstractVerticle() {
    override fun start(startPromise: Promise<Void>) {
        val defaultHandler = DefaultHandler()
        val messageHandler = MessageHandler()

        val router = Router.router(vertx)

        router
            .get("/plaintext")
            .handler(defaultHandler::plaintext)

        router
            .get("/json")
            .handler(messageHandler::readDefaultMessage)

        val server = vertx
            .createHttpServer(
                httpServerOptionsOf(
                    port = 8080,
                    tcpKeepAlive = true,
                    tcpFastOpen = true,
                    tcpNoDelay = true,
                    tcpQuickAck = true,
                    reuseAddress = true,
                    reusePort = true,
                    acceptBacklog = BACKLOG,
                    receiveBufferSize = RECV_BUFFER_SIZE,
                    sendBufferSize = SEND_BUFFER_SIZE,
                )
            )
            .requestHandler(router)
            .exceptionHandler {
                if (it.isConnectionReset()) return@exceptionHandler
                logger.error(it) { "Exception in HttpServer" }
            }

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

    companion object : Logging {
        private const val BACKLOG = 4096
        private const val RECV_BUFFER_SIZE = 1 * 1024 * 1024 // 1 MB
        private const val SEND_BUFFER_SIZE = 1 * 1024 * 1024 // 1 MB
    }
}