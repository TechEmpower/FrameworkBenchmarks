package com.example.starter.handlers

import com.example.starter.helpers.PeriodicResolver
import com.example.starter.helpers.Properties
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpHeaderValues
import io.vertx.core.Future
import io.vertx.core.MultiMap
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServerRequest
import io.vertx.core.http.HttpServerResponse
import io.vertx.core.internal.buffer.BufferInternal

@Suppress("NOTHING_TO_INLINE")
abstract class AbstractHandler {
    protected companion object {
        val SOMETHING_WENT_WRONG: Buffer = BufferInternal.buffer("Something went wrong")

        // Headers
        val SERVER: CharSequence = HttpHeaders.createOptimized(Properties.SERVER_NAME)

        inline fun MultiMap.common(): MultiMap = this
            .add(HttpHeaderNames.SERVER, SERVER)
            .add(HttpHeaderNames.DATE, PeriodicResolver.current)

        inline fun HttpServerRequest.json(): HttpServerResponse = response()
            .apply {
                headers()
                    .common()
                    .add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.APPLICATION_JSON)
            }

        inline fun HttpServerRequest.plaintext(): HttpServerResponse = response()
            .apply {
                headers()
                    .common()
                    .add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_PLAIN)
            }

        inline fun HttpServerRequest.html(): HttpServerResponse = response()
            .apply {
                headers()
                    .common()
                    .add(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_HTML)
            }

        inline fun HttpServerRequest.error(): Future<Void> = plaintext()
            .setStatusCode(500)
            .end(SOMETHING_WENT_WRONG)
    }
}