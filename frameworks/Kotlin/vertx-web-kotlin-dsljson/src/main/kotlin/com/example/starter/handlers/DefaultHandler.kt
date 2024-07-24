package com.example.starter.handlers

import io.vertx.core.buffer.Buffer
import io.vertx.ext.web.RoutingContext

class DefaultHandler : AbstractHandler() {
    fun plaintext(ctx: RoutingContext) {
        ctx.text().end(MESSAGE_BUFFER, NULL_HANDLER)
    }

    companion object {
        private const val MESSAGE = "Hello, World!"
        private val MESSAGE_BUFFER = Buffer.buffer(MESSAGE, "UTF-8")
    }
}
