import io.netty.channel.unix.Errors
import io.netty.channel.unix.Errors.NativeIoException
import io.vertx.core.MultiMap
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServer
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.coroutines.CoroutineRouterSupport
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.coAwait
import kotlinx.coroutines.Dispatchers
import kotlinx.datetime.UtcOffset
import kotlinx.datetime.format.DateTimeComponents
import kotlinx.datetime.format.format
import kotlinx.io.buffered
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationStrategy
import kotlinx.serialization.json.io.encodeToSink
import java.net.SocketException
import kotlin.random.Random
import kotlin.time.Clock

abstract class CommonVerticle : CoroutineVerticle(), CoroutineRouterSupport {
    object HttpHeaderValues {
        val vertxWeb = HttpHeaders.createOptimized("Vert.x-Web")
        val applicationJson = HttpHeaders.createOptimized("application/json")
        val textHtmlCharsetUtf8 = HttpHeaders.createOptimized("text/html; charset=utf-8")
        val textPlain = HttpHeaders.createOptimized("text/plain")
    }

    lateinit var httpServer: HttpServer
    lateinit var date: String
    val random = Random(0)

    fun setCurrentDate() {
        date = DateTimeComponents.Formats.RFC_1123.format {
            // We don't need a more complicated system `TimeZone` here (whose offset depends dynamically on the actual time due to DST) since UTC works.
            setDateTimeOffset(Clock.System.now(), UtcOffset.ZERO)
        }
    }

    override suspend fun start() {
        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(
            httpServerOptionsOf(
                port = 8080,
                http2ClearTextEnabled = false,
                strictThreadMode = httpServerStrictThreadMode
            )
        )
            .requestHandler(Router.router(vertx).apply { routes() })
            .exceptionHandler {
                // wrk resets the connections when benchmarking is finished.
                if ((/* for native transport */it is NativeIoException && it.expectedErr() == Errors.ERRNO_ECONNRESET_NEGATIVE) ||
                    (/* for Java NIO */ it is SocketException && it.message == "Connection reset")
                )
                    return@exceptionHandler

                logger.info("Exception in HttpServer: $it")
                it.printStackTrace()
            }
            .listen().coAwait()
    }

    // set to `false` to resolve `java.lang.IllegalStateException: Only the context thread can write a message
    open val httpServerStrictThreadMode get() = true

    abstract fun Router.routes()

    @Suppress("NOTHING_TO_INLINE")
    inline fun MultiMap.addCommonHeaders() {
        add(HttpHeaders.SERVER, HttpHeaderValues.vertxWeb)
        add(HttpHeaders.DATE, date)
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.addJsonResponseHeaders() {
        headers().run {
            addCommonHeaders()
            add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.applicationJson)
        }
    }


    fun Route.coHandlerUnconfined(requestHandler: suspend (RoutingContext) -> Unit): Route =
        /* Some conclusions from the Plaintext test results with trailing `await()`s:
           1. `launch { /*...*/ }` < `launch(start = CoroutineStart.UNDISPATCHED) { /*...*/ }` < `launch(Dispatchers.Unconfined) { /*...*/ }`.
           1. `launch { /*...*/ }` without `context` or `start` lead to `io.netty.channel.StacklessClosedChannelException` and `io.netty.channel.unix.Errors$NativeIoException: sendAddress(..) failed: Connection reset by peer`. */
        coHandler(Dispatchers.Unconfined, requestHandler)

    // an alternative way
    // set to `EmptyCoroutineContext` to resolve `java.lang.IllegalStateException: Only the context thread can write a message
    //open val coHandlerCoroutineContext: CoroutineContext get() = Dispatchers.Unconfined

    inline fun <reified T : Any> Route.jsonResponseCoHandler(
        serializer: SerializationStrategy<T>,
        crossinline requestHandler: suspend (RoutingContext) -> @Serializable T
    ) =
        coHandlerUnconfined {
            it.response().run {
                addJsonResponseHeaders()

                /*
                // Approach 1
                end(json.encodeToString(serializer, requestHandler(it)))/*.coAwait()*/
                */

                /*
                // Approach 2
                // java.lang.IllegalStateException: You must set the Content-Length header to be the total size of the message body BEFORE sending any data if you are not using HTTP chunked encoding.
                toRawSink().buffered().use { bufferedSink ->
                    @OptIn(ExperimentalSerializationApi::class)
                    json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                }
                */

                // Approach 3
                end(Buffer.buffer().apply {
                    toRawSink().buffered().use { bufferedSink ->
                        @OptIn(ExperimentalSerializationApi::class)
                        json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                    }
                })
            }
        }
}