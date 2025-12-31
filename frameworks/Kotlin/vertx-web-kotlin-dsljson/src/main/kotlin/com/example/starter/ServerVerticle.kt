package com.example.starter

import com.example.starter.db.FortuneRepository
import com.example.starter.db.WorldRepository
import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.FortuneHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.handlers.WorldHandler
import com.example.starter.helpers.Properties
import com.example.starter.utils.isConnectionReset
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.coAwait
import io.vertx.pgclient.PgConnection
import org.apache.logging.log4j.kotlin.Logging

class ServerVerticle : CoroutineVerticle() {
    override suspend fun start() {
        val conn = PgConnection.connect(vertx, Properties.PG_CONNECT).coAwait()

        val fortuneRepository = FortuneRepository.init(conn)
        val worldRepository = WorldRepository.init(conn)

        val fortuneHandler = FortuneHandler(fortuneRepository.coAwait())
        val worldHandler = WorldHandler(worldRepository.coAwait())

        val defaultHandler = DefaultHandler()
        val messageHandler = MessageHandler()

        val server = vertx
            .createHttpServer(Properties.HTTP)
            .requestHandler {
                val path = it.path()
                val code = when (path.length) {
                    10 -> if (path == PLAINTEXT_PATH) 1 else 0
                    5  -> if (path == JSON_PATH)      2 else 0
                    9  -> if (path == FORTUNES_PATH)  3 else 0
                    3  -> if (path == DB_PATH)        4 else 0
                    8  -> when (path) {
                        QUERIES_PATH -> 5
                        UPDATES_PATH -> 6
                        else         -> 0
                    }
                    else -> 0
                }
                when (code) {
                    1 -> defaultHandler.plaintext(it)
                    2 -> messageHandler.readDefaultMessage(it)
                    3 -> fortuneHandler.templateFortunes(it)
                    4 -> worldHandler.readRandomWorld(it)
                    5 -> worldHandler.readRandomWorlds(it)
                    6 -> worldHandler.updateRandomWorlds(it)
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
        private const val FORTUNES_PATH = "/fortunes"
        private const val DB_PATH = "/db"
        private const val QUERIES_PATH = "/queries"
        private const val UPDATES_PATH = "/updates"
    }
}
