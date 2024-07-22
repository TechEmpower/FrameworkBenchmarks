package com.example.starter.utils

import io.vertx.core.Vertx
import java.util.concurrent.TimeUnit
import kotlin.time.Duration

fun Vertx.awaitClose(duration: Duration) {
    this.close()
        .toCompletionStage()
        .toCompletableFuture()
        .get(duration.inWholeMilliseconds, TimeUnit.MILLISECONDS)
}