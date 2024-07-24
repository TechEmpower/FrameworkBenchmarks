package com.example.starter

import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.io.JsonResource
import com.example.starter.utils.isConnectionReset
import io.vertx.core.AbstractVerticle
import io.vertx.core.Promise
import io.vertx.core.http.HttpServerOptions
import io.vertx.ext.web.Router
import org.apache.logging.log4j.kotlin.Logging

class BasicVerticle : AbstractVerticle() {
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
            .createHttpServer(HTTP_SERVER_OPTIONS)
            .requestHandler(router)
            .exceptionHandler {
                if (it.isConnectionReset()) return@exceptionHandler
                logger.error(it) { "Exception in HttpServer" }
            }

        server
            .listen()
            .onSuccess {
                logger.info { "HTTP server started on port 8080" }
                startPromise.complete()
            }
            .onFailure {
                logger.error(it.cause) { "Failed to start" }
                startPromise.fail(it.cause)
            }
    }

    companion object : Logging {
        private const val HTTP_SERVER_OPTIONS_RESOURCE = "http-server-options.json"

        private val HTTP_SERVER_OPTIONS: HttpServerOptions by lazy {
            val json = JsonResource.of(HTTP_SERVER_OPTIONS_RESOURCE)
            HttpServerOptions(json)
        }
    }
}
