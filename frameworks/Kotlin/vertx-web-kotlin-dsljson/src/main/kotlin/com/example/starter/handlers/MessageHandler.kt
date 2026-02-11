package com.example.starter.handlers

import com.example.starter.helpers.PeriodicResolver
import com.example.starter.models.Message
import com.example.starter.utils.serialize
import io.vertx.core.Future
import io.vertx.core.http.HttpServerRequest

class MessageHandler : AbstractHandler() {
    fun readDefaultMessage(req: HttpServerRequest): Future<Void> = req
        .response().apply {
            headers().setAll(PeriodicResolver.json)
        }
        .end(Message(MESSAGE).serialize())

    companion object {
        const val MESSAGE: String = "Hello, World!"
    }
}
