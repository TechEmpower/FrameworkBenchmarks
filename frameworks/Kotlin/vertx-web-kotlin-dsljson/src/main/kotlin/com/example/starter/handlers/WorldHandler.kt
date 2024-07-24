package com.example.starter.handlers

import com.example.starter.db.WorldRepository
import com.example.starter.utils.serialize
import io.vertx.ext.web.RoutingContext
import org.apache.logging.log4j.kotlin.Logging

class WorldHandler(private val repository: WorldRepository) : AbstractHandler() {
    fun readRandomWorld(ctx: RoutingContext) {
        repository
            .selectRandomWorld()
            .onSuccess {
                ctx.json().end(it.serialize(), NULL_HANDLER)
            }
            .onFailure {
                logger.error(it) { SOMETHING_WENT_WRONG }
                ctx.error()
            }
    }

    fun readRandomWorlds(ctx: RoutingContext) {
        val queries = ctx.queries()
        repository
            .selectRandomWorlds(queries)
            .onSuccess {
                ctx.json().end(it.serialize(), NULL_HANDLER)
            }
            .onFailure {
                logger.error(it) { SOMETHING_WENT_WRONG }
                ctx.error()
            }
    }

    fun updateRandomWorlds(ctx: RoutingContext) {
        val queries = ctx.queries()
        repository
            .updateRandomWorlds(queries)
            .onSuccess {
                ctx.json().end(it.serialize(), NULL_HANDLER)
            }
            .onFailure {
                logger.error(it) { SOMETHING_WENT_WRONG }
                ctx.error()
            }
    }

    companion object : Logging {
        private const val QUERIES_PARAM_NAME = "queries"

        @Suppress("NOTHING_TO_INLINE")
        private inline fun RoutingContext.queries(): Int {
            val queriesParam = this.request().getParam(QUERIES_PARAM_NAME)
            return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
        }
    }
}