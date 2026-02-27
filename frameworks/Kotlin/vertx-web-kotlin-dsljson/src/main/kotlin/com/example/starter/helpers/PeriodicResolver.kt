package com.example.starter.helpers

import io.vertx.core.MultiMap
import io.vertx.core.Vertx
import io.vertx.core.http.HttpHeaders
import java.time.Instant
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

private val FORMATTER = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)
private val SERVER: CharSequence = HttpHeaders.createOptimized(Properties.SERVER_NAME)
private val CONTENT_TYPE_TEXT_PLAIN: CharSequence = HttpHeaders.createOptimized("text/plain")
private val CONTENT_TYPE_APPLICATION_JSON: CharSequence = HttpHeaders.createOptimized("application/json")

@Suppress("NOTHING_TO_INLINE")
object PeriodicResolver {
    @Volatile
    var current: CharSequence = next()
        private set

    @Volatile
    var plaintext: MultiMap = nextPlaintext()
        private set

    @Volatile
    var json: MultiMap = nextJson()
        private set

    fun init(vertx: Vertx) {
        vertx.setPeriodic(1_000L) {
            current = next()
            plaintext = nextPlaintext()
            json = nextJson()
        }
    }

    private fun next(): CharSequence = HttpHeaders.createOptimized(
        FORMATTER.format(Instant.now())
    )

    private fun nextPlaintext(): MultiMap = HttpHeaders
        .headers()
        .add(HttpHeaders.SERVER, SERVER)
        .add(HttpHeaders.DATE, current)
        .add(HttpHeaders.CONTENT_TYPE, CONTENT_TYPE_TEXT_PLAIN)
        .copy(false)

    private fun nextJson(): MultiMap = HttpHeaders
        .headers()
        .add(HttpHeaders.SERVER, SERVER)
        .add(HttpHeaders.DATE, current)
        .add(HttpHeaders.CONTENT_TYPE, CONTENT_TYPE_APPLICATION_JSON)
        .copy(false)
}