
import com.fasterxml.jackson.databind.JsonNode
import org.http4k.core.Body
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.NOT_FOUND
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.format.Jackson.array
import org.http4k.format.Jackson.json
import org.http4k.format.Jackson.number
import org.http4k.format.Jackson.obj
import org.http4k.lens.Query
import org.http4k.routing.bind
import java.lang.Math.max
import java.lang.Math.min
import java.sql.Connection
import java.util.Random


object WorldRoutes {

    private val jsonBody = Body.json().toLens()

    private val numberOfQueries = Query
        .map {
            try {
                min(max(it.toInt(), 1), 500)
            } catch (e: Exception) {
                1
            }
        }
        .defaulted("queries", 1)

    fun queryRoute(database: Database) = "/db" bind GET to {
        database.withConnection {
            findWorld(randomWorld())
        }?.let { Response(OK).with(jsonBody of it) } ?: Response(NOT_FOUND)
    }

    fun multipleRoute(database: Database) = "/queries" bind GET to {
        val worlds = database.withConnection {
            (1..numberOfQueries(it)).mapNotNull { findWorld(randomWorld()) }
        }
        Response(OK).with(jsonBody of array(worlds))
    }

    fun updateRoute(database: Database) = "/updates" bind GET to {
        val worlds = database.withConnection {
            (1..numberOfQueries(it)).mapNotNull {
                val id = randomWorld()
                updateWorld(id)
                findWorld(id)
            }
        }
        Response(OK).with(jsonBody of array(worlds))
    }

    private fun Connection.findWorld(id: Int): JsonNode? =
        withStatement("SELECT * FROM world WHERE id = ?") {
            setInt(1, id)
            executeQuery().toList {
                obj("id" to number(getInt("id")), "randomNumber" to number(getInt("randomNumber")))
            }.firstOrNull()
        }

    private fun Connection.updateWorld(id: Int) = withStatement("UPDATE world SET randomNumber = ? WHERE id = ?") {
        setInt(1, randomWorld())
        setInt(2, id)
        executeUpdate()
    }

    private fun randomWorld() = Random().nextInt(9999) + 1
}