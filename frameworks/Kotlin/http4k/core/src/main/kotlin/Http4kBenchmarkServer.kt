import org.apache.commons.lang3.time.FastDateFormat.getInstance
import org.http4k.core.Filter
import org.http4k.core.HttpHandler
import org.http4k.core.then
import org.http4k.routing.routes
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.util.TimeZone.getTimeZone

object Http4kBenchmarkServer {
    private val dateFormat = getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

    private fun headers(addDate: Boolean) = Filter { next ->
        {
            next(it).let {
                it.headers(listOf(
                    "Server" to "http4k",
                    "Content-Length" to it.body.length.toString(),
                    "Date" to if (addDate) dateFormat.format(System.currentTimeMillis()) else null
                ))
            }
        }
    }

    operator fun invoke(database: Database, addDateHeader: Boolean = true) =
        headers(addDateHeader).then(
            routes(
                JsonRoute(),
                PlainTextRoute(),
                FortunesRoute(database),
                WorldRoutes.queryRoute(database),
                WorldRoutes.updateRoute(database),
                WorldRoutes.multipleRoute(database),
                WorldRoutes.cachedRoute(database)
            )
        )
}

fun HttpHandler.start(config: ServerConfig) = asServer(config).start().block()
