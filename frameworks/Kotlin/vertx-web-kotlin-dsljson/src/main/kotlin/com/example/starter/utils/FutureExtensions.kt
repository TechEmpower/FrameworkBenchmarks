package com.example.starter.utils

import io.vertx.core.CompositeFuture

inline fun <reified T> CompositeFuture.array(): Array<T> = Array(this.size()) { this.resultAt(it) }