import io.vertx.core.Future
import io.vertx.kotlin.coroutines.coAwait

suspend fun <T> List<Future<T>>.awaitAll(): List<T> =
    Future.all(this).coAwait().list()
