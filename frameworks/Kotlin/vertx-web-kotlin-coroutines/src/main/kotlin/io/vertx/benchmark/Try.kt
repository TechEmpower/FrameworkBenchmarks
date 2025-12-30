package io.vertx.benchmark

sealed class Try<out T> {
    class Success<out T>(val value: T) : Try<T>()
    class Failure(val throwable: Throwable) : Try<Nothing>()
}

inline fun <T> `try`(block: () -> T): Try<T> =
    try {
        Try.Success(block())
    } catch (t: Throwable) {
        Try.Failure(t)
    }