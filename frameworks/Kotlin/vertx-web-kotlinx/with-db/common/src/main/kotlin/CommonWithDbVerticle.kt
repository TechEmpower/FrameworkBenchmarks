import io.vertx.core.http.HttpHeaders
import io.vertx.core.http.HttpServerRequest
import io.vertx.ext.web.Router
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.html.*
import kotlinx.html.stream.appendHTML
import kotlin.random.Random

interface CommonWithDbVerticleI<out DbClient : Any, OptionalTransaction> : CoroutineScope {
    /**
     * Override this method to run [block] with a transaction.
     * The methods below [selectWorld], [selectRandomWorlds], and [updateSortedWorlds] are all run in this [block],
     * and should not create transactions on their own.
     */
    suspend fun <T> withOptionalTransaction(block: suspend OptionalTransaction.() -> T): T


    suspend fun OptionalTransaction.selectWorld(id: Int): World

    val random: Random

    suspend fun OptionalTransaction.selectRandomWorld() =
        selectWorld(random.nextIntBetween1And10000())

    suspend fun OptionalTransaction.selectRandomWorlds(queries: Int): List<World>

    suspend fun OptionalTransaction.updateSortedWorlds(sortedWorlds: List<World>)

    suspend fun OptionalTransaction.selectFortunesInto(fortunes: MutableList<Fortune>)


    interface ParallelOrPipelinedSelectWorlds<DbClient : Any, OptionalTransaction> :
        CommonWithDbVerticleI<DbClient, OptionalTransaction> {
        override suspend fun OptionalTransaction.selectRandomWorlds(queries: Int): List<World> =
        //List(queries) { async { selectRandomWorld() } }.awaitAll()
            // This should be slightly more efficient.
            awaitAll(*Array(queries) { async { selectRandomWorld() } })
    }

    interface SequentialSelectWorlds<DbClient : Any, OptionalTransaction> :
        CommonWithDbVerticleI<DbClient, OptionalTransaction> {
        override suspend fun OptionalTransaction.selectRandomWorlds(queries: Int): List<World> =
            List(queries) { selectRandomWorld() }
    }

    //interface WithoutTransaction : CommonWithDbVerticleI<Any, Unit> { // This doesn't work in Kotlin.
    interface WithoutTransaction<DbClient : Any> : CommonWithDbVerticleI<DbClient, Unit> {
        override suspend fun <T> withOptionalTransaction(block: suspend Unit.() -> T): T =
            Unit.block()

        /*
        // add some shortcuts, but too verbose in Kotlin with duplicate function names
        override suspend fun Unit.selectWorld(id: Int): World =
            invokeSelectWorld(id)
        suspend fun invokeSelectWorld(id: Int): World =
            selectWorld(id)
        suspend fun selectWorld(id: Int): World
        */
    }
}

abstract class CommonWithDbVerticle<DbClient : Any, Transaction> : CommonVerticle(),
    CommonWithDbVerticleI<DbClient, Transaction> {
    private lateinit var _dbClient: DbClient
    val dbClient: DbClient get() = _dbClient

    override suspend fun start() {
        _dbClient = initDbClient()
        super.start()
    }

    abstract suspend fun initDbClient(): DbClient

    fun HttpServerRequest.getQueries(): Int {
        val queriesParam: String? = getParam("queries")
        return queriesParam?.toIntOrNull()?.coerceIn(1, 500) ?: 1
    }

    override fun Router.routes() {
        get("/db").jsonResponseCoHandler(Serializers.world) {
            withOptionalTransaction {
                selectRandomWorld()
            }
        }

        get("/queries").jsonResponseCoHandler(Serializers.worlds) {
            val queries = it.request().getQueries()
            withOptionalTransaction {
                selectRandomWorlds(queries)
            }
        }

        get("/fortunes").coHandlerUnconfined {
            val fortunes = mutableListOf<Fortune>()
            withOptionalTransaction {
                selectFortunesInto(fortunes)
            }

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
            withOptionalTransaction {
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
}