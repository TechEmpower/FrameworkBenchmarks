import com.huanshankeji.exposedvertxsqlclient.DatabaseClient
import com.huanshankeji.exposedvertxsqlclient.postgresql.PgDatabaseClientConfig
import com.huanshankeji.exposedvertxsqlclient.postgresql.vertx.pgclient.createPgConnection
import database.*
import io.netty.channel.unix.Errors
import io.netty.channel.unix.Errors.NativeIoException
import io.vertx.core.MultiMap
import io.vertx.core.buffer.Buffer
import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServer
import io.vertx.core.http.HttpServerRequest
import io.vertx.core.http.HttpServerResponse
import io.vertx.ext.web.Route
import io.vertx.ext.web.Router
import io.vertx.ext.web.RoutingContext
import io.vertx.kotlin.core.http.httpServerOptionsOf
import io.vertx.kotlin.coroutines.CoroutineRouterSupport
import io.vertx.kotlin.coroutines.CoroutineVerticle
import io.vertx.kotlin.coroutines.coAwait
import io.vertx.pgclient.PgConnection
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.datetime.UtcOffset
import kotlinx.datetime.format.DateTimeComponents
import kotlinx.datetime.format.format
import kotlinx.html.*
import kotlinx.html.stream.appendHTML
import kotlinx.io.buffered
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationStrategy
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.io.encodeToSink
import org.jetbrains.exposed.v1.core.eq
import org.jetbrains.exposed.v1.core.statements.buildStatement
import org.jetbrains.exposed.v1.jdbc.Database
import org.jetbrains.exposed.v1.jdbc.select
import java.net.SocketException
import kotlin.time.Clock

/**
 * @param exposedDatabase `null` indicates that the database is not needed in the test.
 */
class MainVerticle(val exposedDatabase: Database?) : CoroutineVerticle(), CoroutineRouterSupport {
    object HttpHeaderValues {
        val vertxWeb = HttpHeaders.createOptimized("Vert.x-Web")
        val applicationJson = HttpHeaders.createOptimized("application/json")
        val textHtmlCharsetUtf8 = HttpHeaders.createOptimized("text/html; charset=utf-8")
        val textPlain = HttpHeaders.createOptimized("text/plain")
    }

    // `PgConnection`s as used in the "vertx" portion offers better performance than `PgPool`s.
    lateinit var databaseClient: DatabaseClient<PgConnection>
    lateinit var date: String
    lateinit var httpServer: HttpServer

    // kept in case we support generating and reusing `PreparedQuery`
    /*
    lateinit var selectWorldQuery: PreparedQuery<RowSet<Row>>
    lateinit var selectFortuneQuery: PreparedQuery<RowSet<Row>>
    lateinit var updateWorldQuery: PreparedQuery<RowSet<Row>>
    */

    fun setCurrentDate() {
        date = DateTimeComponents.Formats.RFC_1123.format {
            // We don't need a more complicated system `TimeZone` here (whose offset depends dynamically on the actual time due to DST) since UTC works.
            setDateTimeOffset(Clock.System.now(), UtcOffset.ZERO)
        }
    }

    val hasDb get() = exposedDatabase !== null

    override suspend fun start() {
        exposedDatabase?.let {
            // Parameters are copied from the "vertx-web" and "vertx" portions.
            val pgConnection = createPgConnection(vertx, connectionConfig, {
                cachePreparedStatements = true
                pipeliningLimit = 256
            })
            // TODO `validateBatch = false`
            databaseClient = DatabaseClient(pgConnection, it, PgDatabaseClientConfig(/*validateBatch = false*/))
        }

        setCurrentDate()
        vertx.setPeriodic(1000) { setCurrentDate() }
        httpServer = vertx.createHttpServer(
            httpServerOptionsOf(port = 8080, http2ClearTextEnabled = false, strictThreadMode = true)
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


    fun HttpServerRequest.getQueries(): Int {
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }

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
        // see the corresponding comment in the `vertx-web-kotlinx` portion for more details
        coHandler(Dispatchers.Unconfined, requestHandler)

    inline fun <reified T : Any> Route.jsonResponseCoHandler(
        serializer: SerializationStrategy<T>,
        crossinline requestHandler: suspend (RoutingContext) -> @Serializable T
    ) =
        coHandlerUnconfined {
            it.response().run {
                addJsonResponseHeaders()

                // Approach 3 from the `vertx-web-kotlinx` portion
                end(Buffer.buffer().apply {
                    toRawSink().buffered().use { bufferedSink ->
                        @OptIn(ExperimentalSerializationApi::class)
                        Json.encodeToSink(serializer, requestHandler(it), bufferedSink)
                    }
                })
            }
        }


    suspend fun selectRandomWorld() =
        databaseClient.executeQuery(selectWorldWithIdQuery(randomIntBetween1And10000()))
            .single().toWorld()

    suspend fun selectRandomWorlds(queries: Int): List<World> =
    //List(queries) { async { selectRandomWorld() } }.awaitAll()
        // This should be slightly more efficient.
        awaitAll(*Array(queries) { async { selectRandomWorld() } })

    fun Router.routes() {
        get("/db").jsonResponseCoHandler(Serializers.world) {
            selectRandomWorld()
        }

        get("/queries").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            selectRandomWorlds(queries)
        }

        get("/fortunes").coHandlerUnconfined {
            val fortunes = mutableListOf<Fortune>()
            databaseClient.executeQuery(with(FortuneTable) { select(id, message) })
                .mapTo(fortunes) { it.toFortune() }

            fortunes.add(Fortune(0, "Additional fortune added at request time."))
            fortunes.sortBy { it.message }

            val htmlString = buildString {
                append("<!DOCTYPE html>")
                appendHTML(false).html {
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
            }

            it.response().run {
                headers().run {
                    addCommonHeaders()
                    add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.textHtmlCharsetUtf8)
                }
                end(htmlString)/*.coAwait()*/
            }
        }

        // Some changes to this part in the `vertx` portion in #9142 are not ported.
        get("/updates").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            val worlds = selectRandomWorlds(queries)
            val updatedWorlds = worlds.map { it.copy(randomNumber = randomIntBetween1And10000()) }

            // Approach 1 from the `vertx-web-kotlinx` portion
            // TODO `sortedBy { it.id }`
            databaseClient.executeBatchUpdate(updatedWorlds.map { world ->
                buildStatement {
                    WorldTable.update({ WorldTable.id eq world.id }) {
                        it[randomNumber] = world.randomNumber
                    }
                }
            })

            updatedWorlds
        }
    }
}