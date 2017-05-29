import org.apache.commons.lang3.time.FastDateFormat.getInstance
import org.http4k.core.Filter
import org.http4k.core.then
import org.http4k.routing.routes
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.util.TimeZone.getTimeZone

object Http4kBenchmarkServer {
    private val dateFormat = getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

    private val dateAndServer = Filter {
        next ->
        {
            next(it)
                .header("Server", "Example")
                .header("Date", dateFormat.format(System.currentTimeMillis()))
        }
    }

    private val database = Database(System.getenv("DBHOST") ?: "localhost")

    val routes = listOf(PlainTextRoute(),
        JsonRoute(),
        FortunesRoute(database)
    ).plus(WorldRoutes(database))

    fun start(config: ServerConfig) = dateAndServer.then(routes(*routes.toTypedArray())).asServer(config).start().block()
}