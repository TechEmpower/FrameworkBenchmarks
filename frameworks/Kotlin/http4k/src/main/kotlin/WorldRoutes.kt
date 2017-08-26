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
import org.http4k.routing.Route
import org.http4k.routing.bind
import java.lang.Math.max
import java.lang.Math.min
import java.sql.Connection
import java.sql.ResultSet.CONCUR_READ_ONLY
import java.sql.ResultSet.TYPE_FORWARD_ONLY
import java.util.*


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

    operator fun invoke(database: Database): List<Route> =
        listOf(
            queryRoute(database),
            multipleRoute(database),
            updateRoute(database)
        )

    private fun queryRoute(database: Database): Route = "/db" to GET bind {
        database.withConnection {
            findWorld(it, randomWorld())
        }?.let { Response(OK).with(jsonBody of it) } ?: Response(NOT_FOUND)
    }

    private fun multipleRoute(database: Database): Route = "/queries" to GET bind {
        val worlds = database.withConnection {
            con ->
            (1..numberOfQueries(it)).mapNotNull { findWorld(con, randomWorld()) }
        }
        Response(OK).with(jsonBody of array(worlds))
    }

    private fun updateRoute(database: Database): Route = "/updates" to GET bind {
        val worlds = database.withConnection {
            con ->
            (1..numberOfQueries(it)).mapNotNull {
                val id = randomWorld()
                updateWorld(con, id)
                findWorld(con, id)
            }
        }
        Response(OK).with(jsonBody of array(worlds))
    }

    private fun findWorld(it: Connection, id: Int): JsonNode? {
        val stmtSelect = it.prepareStatement("select * from world where id = ?", TYPE_FORWARD_ONLY, CONCUR_READ_ONLY)
        stmtSelect.setInt(1, id)
        return stmtSelect.executeQuery().toList {
            obj("id" to number(it.getInt("id")), "randomNumber" to number(it.getInt("randomNumber")))
        }.firstOrNull()
    }

    private fun updateWorld(it: Connection, id: Int) {
        val stmtSelect = it.prepareStatement("update world set randomNumber = ? where id = ?")
        stmtSelect.setInt(1, randomWorld())
        stmtSelect.setInt(2, id)
        stmtSelect.executeUpdate()
    }

    private fun randomWorld() = Random().nextInt(9999) + 1
}