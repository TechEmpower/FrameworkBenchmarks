import org.apache.commons.lang3.time.FastDateFormat.getInstance
import org.http4k.core.Filter
import org.http4k.core.then
import org.http4k.routing.routes
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.util.TimeZone.getTimeZone

object Http4kBenchmarkServer {
    private val dateFormat = getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

    private val headers = Filter { next ->
        {
            next(it).let {
                it.headers(listOf(
                    "Server" to "http4k",
                    "Date" to dateFormat.format(System.currentTimeMillis()),
                    "Content-Length" to it.body.length.toString()))
            }
        }
    }

    private val database = Database("tfb-database")

    fun start(config: ServerConfig) = headers.then(
        routes(
            JsonRoute(),
            PlainTextRoute(),
            FortunesRoute(database),
            WorldRoutes.queryRoute(database),
            WorldRoutes.updateRoute(database),
            WorldRoutes.multipleRoute(database)
        )
    ).asServer(config).start().block()
}