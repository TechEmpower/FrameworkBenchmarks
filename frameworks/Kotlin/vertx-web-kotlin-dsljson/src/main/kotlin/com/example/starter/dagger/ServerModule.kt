package com.example.starter.dagger

import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.FortuneHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.handlers.WorldHandler
import com.example.starter.utils.isConnectionReset
import dagger.Module
import dagger.Provides
import io.vertx.core.Vertx
import io.vertx.core.http.HttpServer
import io.vertx.ext.web.Router
import io.vertx.kotlin.core.http.httpServerOptionsOf
import jakarta.inject.Singleton
import org.apache.logging.log4j.kotlin.loggerOf

@Module
class ServerModule {
    @Provides
    @Singleton
    fun provideRouter(
        vertx: Vertx,
        defaultHandler: DefaultHandler,
        fortuneHandler: FortuneHandler,
        messageHandler: MessageHandler,
        worldHandler: WorldHandler
    ): Router {
        val router = Router.router(vertx)

        router
            .get("/plaintext")
            .handler(defaultHandler::plaintext)

        router
            .get("/fortunes")
            .handler(fortuneHandler::templateFortunes)

        router
            .get("/json")
            .handler(messageHandler::readDefaultMessage)

        router
            .get("/db")
            .handler(worldHandler::readRandomWorld)

        router
            .get("/queries")
            .handler(worldHandler::readRandomWorlds)

        router
            .get("/updates")
            .handler(worldHandler::updateRandomWorlds)

        return router
    }

    @Provides
    @Singleton
    fun provideServer(vertx: Vertx, router: Router): HttpServer = vertx
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

    companion object {
        private val logger = loggerOf(HttpServer::class.java)

        private const val BACKLOG = 4096
        private const val RECV_BUFFER_SIZE = 1 * 1024 * 1024 // 1 MB
        private const val SEND_BUFFER_SIZE = 1 * 1024 * 1024 // 1 MB
    }
}
