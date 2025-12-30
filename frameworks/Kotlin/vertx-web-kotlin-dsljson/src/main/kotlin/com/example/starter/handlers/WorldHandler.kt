package com.example.starter.handlers

import com.example.starter.db.WorldRepository
import com.example.starter.utils.serialize
import io.vertx.core.http.HttpServerRequest
import org.apache.logging.log4j.kotlin.Logging

class WorldHandler(private val repository: WorldRepository) : AbstractHandler() {
    fun readRandomWorld(req: HttpServerRequest) {
        repository
            .selectRandomWorld()
            .onComplete { ar ->
                when {
                    ar.succeeded() -> req.json().end(ar.result().serialize())
                    else -> {
                        logger.error(SOMETHING_WENT_WRONG, ar.cause())
                        req.error()
                    }
                }
            }
    }

    fun readRandomWorlds(req: HttpServerRequest) {
        repository
            .selectRandomWorlds(req.queries())
            .onComplete { ar ->
                when {
                    ar.succeeded() -> req.json().end(ar.result().serialize())
                    else -> {
                        logger.error(SOMETHING_WENT_WRONG, ar.cause())
                        req.error()
                    }
                }
            }
    }

    fun updateRandomWorlds(req: HttpServerRequest) {
        repository
            .updateRandomWorlds(req.queries())
            .onComplete { ar ->
                when {
                    ar.succeeded() -> req.json().end(ar.result().serialize())
                    else -> {
                        logger.error(SOMETHING_WENT_WRONG, ar.cause())
                        req.error()
                    }
                }
            }
    }

    private companion object : Logging {
        private const val QUERIES_PARAM_NAME = "queries"

        @Suppress("NOTHING_TO_INLINE")
        private inline fun HttpServerRequest.queries(): Int = getParam(QUERIES_PARAM_NAME)
            ?.toIntOrNull()
            ?.coerceIn(1, 500) ?: 1
    }
}