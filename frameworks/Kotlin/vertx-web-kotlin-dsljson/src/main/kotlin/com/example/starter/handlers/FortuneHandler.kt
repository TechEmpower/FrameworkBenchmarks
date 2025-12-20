package com.example.starter.handlers

import com.example.starter.db.FortuneRepository
import com.example.starter.models.Fortune
import io.vertx.core.http.HttpServerRequest
import java.lang.Exception
import org.apache.logging.log4j.kotlin.Logging

class FortuneHandler(private val repository: FortuneRepository) : AbstractHandler() {
    fun templateFortunes(req: HttpServerRequest) {
        repository
            .selectFortunes()
            .onComplete { ar ->
                when {
                    ar.succeeded() -> {
                        try {
                            val updatedFortunes = ar.result().plus(Fortune(0, "Additional fortune added at request time."))
                            updatedFortunes.sort()

                            val html = renderFortunes(updatedFortunes)
                            req.html().end(html)
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

    private companion object : Logging {
        private fun renderFortunes(fortunes: Array<Fortune>): String {
            throw NotImplementedError("Not implemented")
        }
    }
}
