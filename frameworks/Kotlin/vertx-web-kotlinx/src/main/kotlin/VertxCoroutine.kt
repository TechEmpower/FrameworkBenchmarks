import io.vertx.core.CompositeFuture
import io.vertx.core.Future
import io.vertx.kotlin.coroutines.await

suspend fun <T> List<Future<T>>.awaitAll(): List<T> =
    CompositeFuture.all(this).await().list()
