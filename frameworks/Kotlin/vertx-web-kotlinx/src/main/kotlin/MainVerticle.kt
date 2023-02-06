import io.netty.channel.unix.Errors.NativeIoException
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServer
import io.vertx.core.http.HttpServerRequest
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.await
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.kotlin.sqlclient.poolOptionsOf
import io.vertx.pgclient.PgPool
import io.vertx.sqlclient.Tuple
import kotlinx.coroutines.*
import kotlinx.html.*
import kotlinx.html.stream.createHTML
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class MainVerticle : CoroutineVerticle() {
    /*
    // TODO: remove the logging code for debugging in the next commit
    val logger = Logger.getLogger("debug")

    enum class State {
        Plaintext, Json, Db, Queries, Fortunes, Updates
    }

    var current: State? = null
    fun switchAndLog(state: State) {
        if (current != state) {
            logger.info("Switching from $current to $state")
            current = state
        }
    }
    */

    inline fun Route.checkedCoroutineHandler(crossinline requestHandler: suspend (RoutingContext) -> Unit): Route =
        handler { ctx ->
            /* Some conclusions from the Plaintext test results with trailing `await()`s:
               1. `launch { /*...*/ }` < `launch(start = CoroutineStart.UNDISPATCHED) { /*...*/ }` < `launch(Dispatchers.Unconfined) { /*...*/ }`.
               1. `launch { /*...*/ }` without `context` or `start` lead to `io.netty.channel.StacklessClosedChannelException` and `io.netty.channel.unix.Errors$NativeIoException: sendAddress(..) failed: Connection reset by peer`. */
            launch(Dispatchers.Unconfined) {
                try {
                    requestHandler(ctx)
                } catch (t: Throwable) {
                    ctx.fail(t)
                }
            }
        }

    lateinit var pgPool: PgPool
    lateinit var date: String
    lateinit var httpServer: HttpServer

    fun setCurrentDate() {
        // kotlinx-datetime doesn't support the format yet.
        //date = Clock.System.now().toString()
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())
    }

    override suspend fun start() {
        // Parameters are copied from the "vertx-web" portion. TODO: tune them if necessary.
        pgPool = PgPool.pool(
            vertx,
            pgConnectOptionsOf(
                database = "hello_world",
                host = "tfb-database",
                user = "benchmarkdbuser",
                password = "benchmarkdbpass",
                cachePreparedStatements = true,
                pipeliningLimit = 100000
            ), poolOptionsOf(maxSize = 4)
        )
        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(httpServerOptionsOf(port = 8080))
            .requestHandler(Router.router(vertx).apply { routes() })
            .exceptionHandler {
                // wrk resets the connections when benchmarking is finished.
                if (it is NativeIoException && it.message == "recvAddress(..) failed: Connection reset by peer")
                    return@exceptionHandler

                logger.info("Exception in HttpServer: $it")
                it.printStackTrace()
            }
            .listen().await()
    }


    fun HttpServerRequest.getQueries(): Int {
        //it.queryParam("queries") // TODO: is this faster?
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }

    // TODO: is using chained calls (fluent API) faster?
    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.putCommonHeaders() {
        putHeader(HttpHeaders.SERVER, "Vert.x Web")
        putHeader(HttpHeaders.DATE, date)
    }

    @Suppress("NOTHING_TO_INLINE")
    inline fun HttpServerResponse.putJsonResponseHeader() {
        putCommonHeaders()
        putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
    }

    inline fun <reified T : Any> Route.jsonResponseHandler(crossinline requestHandler: suspend (RoutingContext) -> @Serializable T) =
        checkedCoroutineHandler {
            it.response().run {
                putJsonResponseHeader()
                // TODO: which is faster? Vert.x or kotlinx.serialization?
                end(Json.encodeToString(requestHandler(it)))/*.await()*/
            }
        }

    // batch execution which seems not permitted
    // TODO: how much faster?
    suspend fun batchSelectRandomWorlds(queries: Int): List<World> =
        pgPool.preparedQuery(SELECT_WORLD_SQL)
            .executeBatch(List(queries) { Tuple.of(randomIntBetween1And10000()) }).await()
            .map { it.toWorld() }

    suspend fun selectRandomWorlds(queries: Int): List<World> {
        val rowSets = List(queries) {
            pgPool.preparedQuery(SELECT_WORLD_SQL).execute(Tuple.of(randomIntBetween1And10000()))
        }.awaitAll()
        return rowSets.map { it.single().toWorld() }
    }

    // TODO: is this approach faster?
    // an alternative approach
    suspend fun selectRandomWorlds2(queries: Int): List<World> =
        List(queries) {
            async {
                val rowSet = pgPool.preparedQuery(SELECT_WORLD_SQL)
                    .execute(Tuple.of(randomIntBetween1And10000())).await()
                rowSet.single().toWorld()
            }
        }.awaitAll()

    fun Router.routes() {
        get("/json").jsonResponseHandler {
            //switchAndLog(State.Json) // TODO: remove the logging code for debugging in the next commit
            jsonSerializationMessage
        }

        get("/db").jsonResponseHandler {
            //switchAndLog(State.Db) // TODO: remove the logging code for debugging in the next commit
            val rowSet = pgPool.preparedQuery(SELECT_WORLD_SQL).execute(Tuple.of(randomIntBetween1And10000())).await()
            // TODO: how much is using `first()` faster?
            rowSet.single().toWorld()
        }

        get("/queries").jsonResponseHandler {
            //switchAndLog(State.Queries) // TODO: remove the logging code for debugging in the next commit
            val queries = it.request().getQueries()
            selectRandomWorlds(queries)
        }

        get("/fortunes").checkedCoroutineHandler {
            //switchAndLog(State.Fortunes) // TODO: remove the logging code for debugging in the next commit
            val fortunes = mutableListOf<Fortune>()
            pgPool.preparedQuery(SELECT_FORTUNE_SQL).execute().await()
                .mapTo(fortunes) { it.toFortune() }

            fortunes.add(Fortune(0, "Additional fortune added at request time."))
            fortunes.sortBy { it.message }

            // TODO: is using `buildString` faster? Refer to https://github.com/Kotlin/kotlinx.html/wiki/Streaming.
            val htmlString = "<!DOCTYPE html>" + createHTML(false).html {
                head {
                    title("Fortunes")
                }
                body {
                    table {
                        tr {
                            th { +"id" }
                            th { +"message" }
                        }
                        for (fortune in fortunes)
                            tr {
                                td { +fortune.id.toString() }
                                td { +fortune.message }
                            }
                    }
                }
            }

            it.response().run {
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=utf-8")
                end(htmlString)/*.await()*/
            }
        }

        get("/updates").jsonResponseHandler {
            //switchAndLog(State.Updates) // TODO: remove the logging code for debugging in the next commit
            val queries = it.request().getQueries()
            val worlds = selectRandomWorlds(queries)
            val updatedWorlds = worlds.map { it.copy(randomNumber = randomIntBetween1And10000()) }

            // Approach 1
            // The updated worlds need to be sorted first to avoid deadlocks.
            pgPool.preparedQuery(UPDATE_WORLD_SQL)
                .executeBatch(updatedWorlds.sortedBy { it.id }.map { Tuple.of(it.randomNumber, it.id) }).await()

            /*
            // Approach 2, worse performance
            updatedWorlds.map {
                pgPool.preparedQuery(UPDATE_WORLD_SQL).execute(Tuple.of(it.randomNumber, it.id))
            }.awaitAll()
            */

            updatedWorlds
        }

        get("/plaintext").checkedCoroutineHandler {
            //switchAndLog(State.Plaintext) // TODO: remove the logging code for debugging in the next commit
            it.response().run {
                putCommonHeaders()
                putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
                // TODO: is reusing a single cached buffer faster?
                end("Hello, World!").await()
            }
        }
    }
}