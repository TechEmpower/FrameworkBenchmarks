package com.example.starter

import com.example.starter.db.FortuneRepository
import com.example.starter.db.WorldRepository
import com.example.starter.handlers.FortuneHandler
import com.example.starter.handlers.WorldHandler
import com.example.starter.io.JsonResource
import com.example.starter.utils.isConnectionReset
import io.vertx.core.AbstractVerticle
import io.vertx.core.Promise
import io.vertx.core.http.HttpServerOptions
import io.vertx.ext.web.Router
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgConnection
import org.apache.logging.log4j.kotlin.Logging

class PostgresVerticle : AbstractVerticle() {
    override fun start(startPromise: Promise<Void>) {
        PgConnection.connect(vertx, PG_CONNECT_OPTIONS)
            .onSuccess { conn ->
                val fortuneHandler = FortuneHandler(FortuneRepository(conn))
                val worldHandler = WorldHandler(WorldRepository(conn))

                val router = Router.router(vertx)

                router
                    .get("/fortunes")
                    .handler(fortuneHandler::templateFortunes)

                router
                    .get("/db")
                    .handler(worldHandler::readRandomWorld)

                router
                    .get("/queries")
                    .handler(worldHandler::readRandomWorlds)

                router
                    .get("/updates")
                    .handler(worldHandler::updateRandomWorlds)

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
                        logger.error(it) { "Failed to start" }
                        startPromise.fail(it)
                    }
            }
            .onFailure {
                logger.error(it) { "Failed to start" }
                startPromise.fail(it)
            }
    }

    companion object : Logging {
        private const val HTTP_SERVER_OPTIONS_RESOURCE = "http-server-options.json"
        private const val PG_CONNECT_OPTIONS_RESOURCE = "pg-connect-options.json"

        private val HTTP_SERVER_OPTIONS: HttpServerOptions by lazy {
            val json = JsonResource.of(HTTP_SERVER_OPTIONS_RESOURCE)
            HttpServerOptions(json)
        }

        private val PG_CONNECT_OPTIONS: PgConnectOptions by lazy {
            val json = JsonResource.of(PG_CONNECT_OPTIONS_RESOURCE)
            PgConnectOptions(json)
        }
    }
}
