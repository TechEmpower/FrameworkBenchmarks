package com.example.starter.utils

import io.vertx.core.Future
import io.vertx.kotlin.coroutines.coAwait
import kotlin.time.Duration
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout

@Suppress("NOTHING_TO_INLINE")
inline fun <R, T: Future<R>> T.block(duration: Duration): R = runBlocking {
    withTimeout(duration) {
        coAwait()
    }
}