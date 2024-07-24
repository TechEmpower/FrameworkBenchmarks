package com.example.starter.utils

import io.vertx.core.Vertx
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object PeriodicDateResolver {
    var current: String = next()

    fun init(vertx: Vertx) {
        vertx.setPeriodic(1000L) { current = next() }
    }

    @Suppress("NOTHING_TO_INLINE")
    private inline fun next(): String = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())
}