package com.example.starter.handlers

import com.example.starter.utils.PeriodicDateResolver
import io.vertx.core.AsyncResult
import io.vertx.core.Handler
import io.vertx.core.MultiMap
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.RoutingContext

@Suppress("NOTHING_TO_INLINE")
abstract class AbstractHandler {
    protected companion object {
        val NULL_HANDLER: Handler<AsyncResult<Void>>? = null

        const val SOMETHING_WENT_WRONG = "Something went wrong"

        // Headers
        val SERVER: CharSequence = HttpHeaders.createOptimized("Vert.x-Web")
        val APPLICATION_JSON: CharSequence = HttpHeaders.createOptimized("application/json")
        val TEXT_PLAIN: CharSequence = HttpHeaders.createOptimized("text/plain")
        val TEXT_HTML: CharSequence = HttpHeaders.createOptimized("text/html; charset=utf-8")

        inline fun MultiMap.common(): MultiMap = this
            .add(HttpHeaders.SERVER, SERVER)
            .add(HttpHeaders.DATE, PeriodicDateResolver.current)

        inline fun RoutingContext.json(): HttpServerResponse {
            val response = this.response()
            val headers = response.headers()
            headers
                .common()
                .add(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
            return response
        }

        inline fun RoutingContext.text(): HttpServerResponse {
            val response = this.response()
            val headers = response.headers()
            headers
                .common()
                .add(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
            return response
        }

        inline fun RoutingContext.html(): HttpServerResponse {
            val response = this.response()
            val headers = response.headers()
            headers
                .common()
                .add(HttpHeaders.CONTENT_TYPE, TEXT_HTML)
            return response
        }

        inline fun RoutingContext.error(): Unit = this.text().end(SOMETHING_WENT_WRONG, NULL_HANDLER)
    }
}