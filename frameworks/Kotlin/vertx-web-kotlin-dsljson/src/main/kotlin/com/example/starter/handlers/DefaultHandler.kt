package com.example.starter.handlers

import com.example.starter.helpers.PeriodicResolver
import io.vertx.core.Future
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpServerRequest

class DefaultHandler : AbstractHandler() {

    fun plaintext(req: HttpServerRequest): Future<Void> = req
        .response().apply {
            headers().setAll(PeriodicResolver.plaintext)
        }
        .end(MESSAGE_BUFFER)

    companion object {
        const val MESSAGE: String = "Hello, World!"
        val MESSAGE_BUFFER: Buffer = Buffer.buffer(MESSAGE, "UTF-8")
    }
}
