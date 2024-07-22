package com.example.starter.handlers

import com.example.starter.models.Message
import com.example.starter.utils.serialize
import io.vertx.ext.web.RoutingContext

class MessageHandler : AbstractHandler() {
    fun readDefaultMessage(ctx: RoutingContext) {
        ctx.json().end(DEFAULT_MESSAGE.serialize(DEFAULT_MESSAGE_SIZE_HINT))
    }

    companion object {
        private val DEFAULT_MESSAGE = Message("Hello, World!")
        private const val DEFAULT_MESSAGE_SIZE_HINT = 28
    }
}