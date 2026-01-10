package com.example.starter.handlers

import FortunesTemplate
import com.example.starter.db.FortuneRepository
import com.example.starter.helpers.BufferRockerOutput
import com.example.starter.models.Fortune
import com.fizzed.rocker.ContentType
import io.vertx.core.http.HttpServerRequest
import org.apache.logging.log4j.kotlin.Logging

class FortuneHandler(private val repository: FortuneRepository) : AbstractHandler() {
    private val factory = BufferRockerOutput.factory(ContentType.RAW)

    fun templateFortunes(req: HttpServerRequest) {
        repository
            .selectFortunes()
            .onComplete { ar ->
                when {
                    ar.succeeded() -> {
                        try {
                            val updatedFortunes = ar.result()
                                .plus(Fortune(0, "Additional fortune added at request time."))
                                .apply { sort() }
                            val buffer = FortunesTemplate.template(updatedFortunes)
                                .render(factory)
                                .buffer()
                            req.html().end(buffer)
                        } catch (ex: Exception) {
                            logger.error(SOMETHING_WENT_WRONG, ex)
                            req.error()
                        }
                    }
                    else -> {
                        logger.error(SOMETHING_WENT_WRONG, ar.cause())
                        req.error()
                    }
                }
            }
    }

    private companion object : Logging
}
