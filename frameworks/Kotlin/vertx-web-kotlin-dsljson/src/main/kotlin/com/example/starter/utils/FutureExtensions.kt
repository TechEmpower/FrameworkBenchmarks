package com.example.starter.utils

import io.vertx.core.CompositeFuture
import io.vertx.core.Future
import java.util.concurrent.TimeUnit
import kotlin.time.Duration

inline fun <reified T> CompositeFuture.array(): Array<T> = Array(this.size()) { this.resultAt(it) }

@Suppress("NOTHING_TO_INLINE")
inline fun <R, T: Future<R>> T.block(duration: Duration): R = this
    .toCompletionStage()
    .toCompletableFuture()
    .get(duration.inWholeMilliseconds, TimeUnit.MILLISECONDS)