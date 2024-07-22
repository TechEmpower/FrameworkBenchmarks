package com.example.starter.utils

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

@Suppress("NOTHING_TO_INLINE")
class PeriodicDateResolver private constructor() {
    private val scheduler = Executors.newSingleThreadScheduledExecutor()
    private var future: ScheduledFuture<*>? = null
    private val lock = ReentrantLock()

    var current: String = next()

    fun start() {
        lock.lock()
        try {
            if (future?.isCancelled != false) {
                future = scheduler.scheduleAtFixedRate({
                    current = next()
                }, PERIOD, PERIOD, TimeUnit.MILLISECONDS)
            }
        } finally {
            lock.unlock()
        }
    }

    fun stop() {
        lock.lock()
        try {
            future?.cancel(false)
            future = null
        } finally {
            lock.unlock()
        }
    }

    companion object {
        private const val PERIOD = 1000L
        private inline fun next(): String = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())

        val INSTANCE: PeriodicDateResolver by lazy { PeriodicDateResolver().apply { start() } }
    }
}