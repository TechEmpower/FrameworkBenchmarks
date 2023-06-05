import org.http4k.core.Body
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.format.Argo.array
import org.http4k.format.Argo.json
import org.http4k.lens.Query
import org.http4k.routing.RoutingHttpHandler
import org.http4k.routing.bind
import kotlin.math.max
import kotlin.math.min

object WorldRoutes {
    private val jsonBody = Body.json().toLens()

    private val numberOfQueries = Query.map {
        try {
            min(max(it.toInt(), 1), 500)
        } catch (e: Exception) {
            1
        }
    }.defaulted("queries", 1)

    fun queryRoute(db: Database) = "/db" bind GET to {
        let { Response(OK).with(jsonBody of db.findWorld()) }
    }

    fun multipleRoute(db: Database) = "/queries" bind GET to {
        Response(OK).with(jsonBody of array(db.findWorlds(numberOfQueries(it))))
    }

    fun cachedRoute(db: Database): RoutingHttpHandler {
        val cachedDb = CachedDatabase(db)

        return "/cached" bind GET to {
            Response(OK).with(jsonBody of array(cachedDb.findWorlds(numberOfQueries(it))))
        }
    }

    fun updateRoute(db: Database) = "/updates" bind GET to {
        Response(OK).with(jsonBody of array(db.updateWorlds(numberOfQueries(it))))
    }
}