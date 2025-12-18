import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServerRequest
import io.vertx.ext.web.Router
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.html.*
import kotlinx.html.stream.appendHTML

abstract class CommonWithDbVerticle<DbClient : Any> : CommonVerticle() {
    lateinit var dbClient: DbClient

    override suspend fun start() {
        dbClient = initDbClient()
        super.start()
    }

    abstract suspend fun initDbClient(): DbClient


    fun HttpServerRequest.getQueries(): Int {
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }


    abstract suspend fun selectWorld(id: Int): World

    suspend fun selectRandomWorld() =
        selectWorld(random.nextIntBetween1And10000())

    suspend fun selectRandomWorlds(queries: Int): List<World> =
    //List(queries) { async { selectRandomWorld() } }.awaitAll()
        // This should be slightly more efficient.
        awaitAll(*Array(queries) { async { selectRandomWorld() } })

    abstract suspend fun updateSortedWorlds(sortedWorlds: List<World>)

    abstract suspend fun selectFortunesInto(fortunes: MutableList<Fortune>)


    override fun Router.routes() {
        get("/db").jsonResponseCoHandler(Serializers.world) {
            selectRandomWorld()
        }

        get("/queries").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            selectRandomWorlds(queries)
        }

        get("/fortunes").coHandlerUnconfined {
            val fortunes = mutableListOf<Fortune>()
            selectFortunesInto(fortunes)

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
            val updatedWorlds = worlds.map { it.copy(randomNumber = random.nextIntBetween1And10000()) }

            // Approach 1
            // The updated worlds need to be sorted first to avoid deadlocks.
            updateSortedWorlds(updatedWorlds.sortedBy { it.id })

            /*
            // Approach 2, worse performance
            updatedWorlds.map {
                pgPool.preparedQuery(UPDATE_WORLD_SQL).execute(Tuple.of(it.randomNumber, it.id))
            }.awaitAll()
            */

            updatedWorlds
        }
    }
}