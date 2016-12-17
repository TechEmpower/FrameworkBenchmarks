package co.there4.hexagon

import co.there4.hexagon.rest.crud
import co.there4.hexagon.serialization.convertToMap
import co.there4.hexagon.serialization.serialize
import co.there4.hexagon.web.*
import co.there4.hexagon.web.servlet.ServletServer
import kotlinx.html.*

import java.net.InetAddress.getByName as address
import java.time.LocalDateTime.now
import java.util.concurrent.ThreadLocalRandom
import javax.servlet.annotation.WebListener

// DATA CLASSES
internal data class Message(val message: String = "Hello, World!")
internal data class Fortune(val _id: Int, val message: String)
internal data class World(val _id: Int, val id: Int = _id, val randomNumber: Int = rnd())

// CONSTANTS
private val CONTENT_TYPE_JSON = "application/json"
private val QUERIES_PARAM = "queries"

private val fortune = Fortune(0, "Additional fortune added at request time.")

// UTILITIES
internal fun rnd() = ThreadLocalRandom.current().nextInt(DB_ROWS) + 1

private fun World.strip(): Map<*, *> = this.convertToMap().filterKeys { it != "_id" }
private fun World.toJson(): String = this.strip().serialize()
private fun List<World>.toJson(): String = this.map(World::strip).serialize()

private fun Exchange.hasQueryCount() = request[QUERIES_PARAM] == null

private fun Exchange.getDb() {
    val worlds = (1..getQueries()).map { findWorld() }.filterNotNull()

    ok(if (hasQueryCount()) worlds[0].toJson() else worlds.toJson(), CONTENT_TYPE_JSON)
}

private fun listFortunes() = (findFortunes() + fortune).sortedBy { it.message }

// HANDLERS
private fun Exchange.getUpdates() {
    val worlds = (1..getQueries()).map {
        val id = rnd()
        val newWorld = World(id, id)
        replaceWorld(newWorld)
        newWorld
    }

    ok(if (hasQueryCount()) worlds[0].toJson() else worlds.toJson(), CONTENT_TYPE_JSON)
}

private fun Exchange.getQueries() =
    try {
        val queries = request[QUERIES_PARAM]?.toInt() ?: 1
        when {
            queries < 1 -> 1
            queries > 500 -> 500
            else -> queries
        }
    }
    catch (ex: NumberFormatException) {
        1
    }

fun benchmarkRoutes(srv: Router = server) {
    srv.before {
        response.addHeader("Server", "Servlet/3.1")
        response.addHeader("Transfer-Encoding", "chunked")
        response.addHeader("Date", httpDate(now()))
    }

    srv.get("/plaintext") { ok("Hello, World!", "text/plain") }
    srv.get("/json") { ok(Message().serialize(), CONTENT_TYPE_JSON) }
    srv.get("/fortunes") { template("fortunes.html", "fortunes" to listFortunes()) }
    srv.get("/db") { getDb() }
    srv.get("/query") { getDb() }
    srv.get("/update") { getUpdates() }
}

@WebListener class Web : ServletServer () {
    override fun init() {
        benchmarkRoutes(this)
    }
}

fun main(args: Array<String>) {
    initialize()
    benchmarkRoutes()

    crud(worldRepository)
    crud(fortuneRepository)

    get("/fortunes_page") {
        page {
            html {
                head {
                    title { +"Fortunes" }
                }
                body {
                    table {
                        tr {
                            th { +"id" }
                            th { +"message" }
                        }
                        listFortunes().forEach {
                            tr {
                                td { +it._id }
                                td { +it.message }
                            }
                        }
                    }
                }
            }
        }
    }

    run()
}
