package com.example.starter.handlers

import com.example.starter.utils.PeriodicDateResolver
import io.vertx.core.Future
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.RoutingContext

@Suppress("NOTHING_TO_INLINE")
abstract class AbstractHandler {
    protected companion object {
        const val SOMETHING_WENT_WRONG = "Something went wrong"

        // Headers
        val SERVER: CharSequence = HttpHeaders.createOptimized("Vert.x-Web")
        val APPLICATION_JSON: CharSequence = HttpHeaders.createOptimized("application/json")
        val TEXT_PLAIN: CharSequence = HttpHeaders.createOptimized("text/plain")
        val TEXT_HTML: CharSequence = HttpHeaders.createOptimized("text/html; charset=utf-8")

        inline fun RoutingContext.respWithCommonHeaders(): HttpServerResponse = this
            .response()
            .putHeader(HttpHeaders.SERVER, SERVER)
            .putHeader(HttpHeaders.DATE, PeriodicDateResolver.INSTANCE.current)

        inline fun RoutingContext.json():HttpServerResponse = this
            .respWithCommonHeaders()
            .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)

        inline fun RoutingContext.text(): HttpServerResponse = this
            .respWithCommonHeaders()
            .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)

        inline fun RoutingContext.html(): HttpServerResponse = this
            .respWithCommonHeaders()
            .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_HTML)

        inline fun RoutingContext.error(): Future<Void> = this.text().end(SOMETHING_WENT_WRONG)
    }
}