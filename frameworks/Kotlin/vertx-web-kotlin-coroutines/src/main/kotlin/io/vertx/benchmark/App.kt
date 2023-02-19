package io.vertx.benchmark

import com.fasterxml.jackson.module.blackbird.BlackbirdModule
import io.vertx.benchmark.model.Fortune
import io.vertx.benchmark.model.Message
import io.vertx.benchmark.model.World
import io.vertx.core.Vertx
import io.vertx.core.http.HttpHeaders
import io.vertx.core.json.Json
import io.vertx.core.json.JsonObject
import io.vertx.core.json.jackson.DatabindCodec
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.ext.web.templ.rocker.RockerTemplateEngine
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.await
import io.vertx.kotlin.pgclient.pgConnectOptionsOf
import io.vertx.pgclient.PgConnection
import io.vertx.sqlclient.Tuple
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.launch
import java.net.UnknownHostException
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import kotlin.system.exitProcess

class App : CoroutineVerticle() {
    companion object {
        init {
            DatabindCodec.mapper().registerModule(BlackbirdModule())
            DatabindCodec.prettyMapper().registerModule(BlackbirdModule())
        }

        private const val SERVER = "vertx-web"

        // for PgClientBenchmark only
        private const val UPDATE_WORLD = "UPDATE world SET randomnumber=$1 WHERE id=$2"
        private const val SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1"
        private const val SELECT_FORTUNE = "SELECT id, message from FORTUNE"
    }

    inline fun Route.coroutineHandlerUnconfined(crossinline requestHandler: suspend (RoutingContext) -> Unit): Route =
        handler { ctx -> launch(Dispatchers.Unconfined) { requestHandler(ctx) } }

    inline fun RoutingContext.checkedRun(block: () -> Unit): Unit =
        try {
            block()
        } catch (t: Throwable) {
            fail(t)
        }

    inline fun Route.checkedCoroutineHandlerUnconfined(crossinline requestHandler: suspend (RoutingContext) -> Unit): Route =
        coroutineHandlerUnconfined { ctx -> ctx.checkedRun { requestHandler(ctx) } }

    suspend fun PgClientBenchmark(vertx: Vertx, config: JsonObject): PgClientBenchmark {
        val options = with(config) {
            pgConnectOptionsOf(
                cachePreparedStatements = true,
                host = getString("host"),
                port = getInteger("port", 5432),
                user = getString("username"),
                password = getString("password"),
                database = config.getString("database"),
                pipeliningLimit = 100000 // Large pipelining means less flushing and we use a single connection anyway;
            )
        }

        return PgClientBenchmark(
            try {
                PgConnection.connect(vertx, options).await()
            } catch (e: UnknownHostException) {
                null
            },
            RockerTemplateEngine.create()
        )
    }

    /**
     * PgClient implementation
     */
    inner class PgClientBenchmark(
        private val client: PgConnection?,
        // In order to use a template we first need to create an engine
        private val engine: RockerTemplateEngine
    ) {
        suspend fun dbHandler(ctx: RoutingContext) {
            val result = try {
                client!!
                    .preparedQuery(SELECT_WORLD)
                    .execute(Tuple.of(randomWorld()))
                    .await()
            } catch (t: Throwable) {
                // adapted from the Java code and kept, though I don't see the purpose of this
                t.printStackTrace()
                throw t
            }

            val resultSet = result.iterator()
            if (!resultSet.hasNext()) {
                ctx.response()
                    .setStatusCode(404)
                    .end()
                    .await()
                return
            }
            val row = resultSet.next()
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(World(row.getInteger(0), row.getInteger(1))))
                .await()
        }

        suspend fun queriesHandler(ctx: RoutingContext) {
            val queries: Int = getQueries(ctx.request())
            val worlds = arrayOfNulls<World>(queries)
            val failed = booleanArrayOf(false)
            val cnt = intArrayOf(0)
            List(queries) {
                async {
                    val result = `try` { client!!.preparedQuery(SELECT_WORLD).execute(Tuple.of(randomWorld())).await() }

                    if (!failed[0]) {
                        if (result is Try.Failure) {
                            failed[0] = true
                            ctx.fail(result.throwable)
                            return@async
                        }

                        // we need a final reference
                        val row = (result as Try.Success).value.iterator().next()
                        worlds[cnt[0]++] = World(row.getInteger(0), row.getInteger(1))

                        // stop condition
                        if (cnt[0] == queries) {
                            ctx.response()
                                .putHeader(HttpHeaders.SERVER, SERVER)
                                .putHeader(HttpHeaders.DATE, date)
                                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                                .end(Json.encodeToBuffer(worlds))
                                .await()
                        }
                    }
                }
            }
                .awaitAll()
        }

        suspend fun fortunesHandler(ctx: RoutingContext) {
            val result = client!!.preparedQuery(SELECT_FORTUNE).execute().await()

            val resultSet = result.iterator()
            if (!resultSet.hasNext()) {
                ctx.fail(404)
                return
            }
            val fortunes = ArrayList<Fortune>()
            while (resultSet.hasNext()) {
                val row = resultSet.next()
                fortunes.add(Fortune(row.getInteger(0), row.getString(1)))
            }
            fortunes.add(Fortune(0, "Additional fortune added at request time."))
            fortunes.sort()
            ctx.put("fortunes", fortunes)

            // and now delegate to the engine to render it.
            val result2 = engine.render(ctx.data(), "templates/Fortunes.rocker.html").await()
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8")
                .end(result2)
                .await()
        }

        suspend fun updateHandler(ctx: RoutingContext) {
            val queries = getQueries(ctx.request())
            val worlds = arrayOfNulls<World>(queries)
            val failed = booleanArrayOf(false)
            val queryCount = intArrayOf(0)
            List(worlds.size) {
                val id = randomWorld()
                async {
                    val r2 = `try` { client!!.preparedQuery(SELECT_WORLD).execute(Tuple.of(id)).await() }

                    if (!failed[0]) {
                        if (r2 is Try.Failure) {
                            failed[0] = true
                            ctx.fail(r2.throwable)
                            return@async
                        }
                        val row = (r2 as Try.Success).value.iterator().next()
                        worlds[queryCount[0]++] = World(row.getInteger(0), randomWorld())
                        if (queryCount[0] == worlds.size) {
                            worlds.sort()
                            val batch = ArrayList<Tuple>()
                            for (world in worlds) {
                                world!!
                                batch.add(Tuple.of(world.randomNumber, world.id))
                            }
                            ctx.checkedRun {
                                client!!.preparedQuery(UPDATE_WORLD)
                                    .executeBatch(batch)
                                    .await()
                                ctx.response()
                                    .putHeader(HttpHeaders.SERVER, SERVER)
                                    .putHeader(HttpHeaders.DATE, date)
                                    .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                                    .end(Json.encodeToBuffer(worlds))
                                    .await()
                            }
                        }
                    }
                }
            }
                .awaitAll()
        }
    }

    private var date: String? = null
    override suspend fun start() {
        val app = Router.router(vertx)
        // initialize the date header
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now())
        // refresh the value as a periodic task
        vertx.setPeriodic(1000) { date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()) }
        val pgClientBenchmark = PgClientBenchmark(vertx, config)

        /*
         * This test exercises the framework fundamentals including keep-alive support, request routing, request header
         * parsing, object instantiation, JSON serialization, response header generation, and request count throughput.
         */
        app.get("/json").checkedCoroutineHandlerUnconfined { ctx ->
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "application/json")
                .end(Json.encodeToBuffer(Message("Hello, World!")))
                .await()
        }

        /*
         * This test exercises the framework's object-relational mapper (ORM), random number generator, database driver,
         * and database connection pool.
         */
        app.get("/db").checkedCoroutineHandlerUnconfined { ctx -> pgClientBenchmark.dbHandler(ctx) }

        /*
         * This test is a variation of Test #2 and also uses the World table. Multiple rows are fetched to more dramatically
         * punish the database driver and connection pool. At the highest queries-per-request tested (20), this test
         * demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.
         */
        app.get("/queries").checkedCoroutineHandlerUnconfined { ctx -> pgClientBenchmark.queriesHandler(ctx) }

        /*
         * This test exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates,
         * XSS countermeasures, and character encoding.
         */
        app.get("/fortunes").checkedCoroutineHandlerUnconfined { ctx -> pgClientBenchmark.fortunesHandler(ctx) }

        /*
         * This test is a variation of Test #3 that exercises the ORM's persistence of objects and the database driver's
         * performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of
         * read-then-write style database operations.
         */
        app.route("/update").checkedCoroutineHandlerUnconfined { ctx -> pgClientBenchmark.updateHandler(ctx) }

        /*
         * This test is an exercise of the request-routing fundamentals only, designed to demonstrate the capacity of
         * high-performance platforms in particular. Requests will be sent using HTTP pipelining. The response payload is
         * still small, meaning good performance is still necessary in order to saturate the gigabit Ethernet of the test
         * environment.
         */
        app.get("/plaintext").checkedCoroutineHandlerUnconfined { ctx ->
            ctx.response()
                .putHeader(HttpHeaders.SERVER, SERVER)
                .putHeader(HttpHeaders.DATE, date)
                .putHeader(HttpHeaders.CONTENT_TYPE, "text/plain")
                .end("Hello, World!")
                .await()
        }
        try {
            vertx.createHttpServer().requestHandler(app).listen(8080).await()
        } catch (t: Throwable) {
            t.printStackTrace()
            exitProcess(1)
        }
    }
}