package com.example.starter.handlers

import com.example.starter.db.FortuneRepository
import com.example.starter.models.Fortune
import htmlflow.HtmlFlow
import htmlflow.HtmlView
import io.vertx.ext.web.RoutingContext
import org.apache.logging.log4j.kotlin.Logging

class FortuneHandler(private val repository: FortuneRepository) : AbstractHandler() {
    fun templateFortunes(ctx: RoutingContext) {
        repository
            .selectFortunes()
            .onSuccess {
                val updatedFortunes = it.plus(Fortune(0, "Additional fortune added at request time."))
                updatedFortunes.sort()
                ctx.html().end(TEMPLATE.render(updatedFortunes), NULL_HANDLER)
            }
            .onFailure {
                logger.error(it) { SOMETHING_WENT_WRONG }
                ctx.error()
            }
    }

    companion object : Logging {
        private val TEMPLATE: HtmlView<Any> = HtmlFlow
            .view<Any> { page ->
                page
                    .html()
                        .head()
                            .title()
                                .text("Fortunes")
                            .`__`() // title
                        .`__`() // head
                        .body()
                            .table()
                                .tr()
                                    .th().text("id").`__`()
                                    .th().text("message").`__`()
                                .`__`() // tr
                                .dynamic<Array<Fortune>> { container, fortunes ->
                                    fortunes.forEach {
                                        container
                                            .tr()
                                                .td().text(it.id.toString()).`__`()
                                                .td().text(it.message).`__`()
                                            .`__`() // tr
                                    }
                                }
                            .`__`() // table
                        .`__`() // body
                    .`__`() // html
            }
            .setIndented(false)
            .threadSafe()
    }
}