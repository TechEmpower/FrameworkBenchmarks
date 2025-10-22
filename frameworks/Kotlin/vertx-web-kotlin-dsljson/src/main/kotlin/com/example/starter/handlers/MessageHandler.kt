package com.example.starter.handlers

import com.example.starter.models.Message
import com.example.starter.utils.serialize
import io.vertx.ext.web.RoutingContext

class MessageHandler : AbstractHandler() {
    fun readDefaultMessage(ctx: RoutingContext) {
        ctx.json().end(DEFAULT_MESSAGE.serialize(), NULL_HANDLER)
    }

    companion object {
        private val DEFAULT_MESSAGE = Message("Hello, World!")
    }
}
