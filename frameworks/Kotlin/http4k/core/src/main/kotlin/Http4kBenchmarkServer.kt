import org.http4k.core.HttpHandler
import org.http4k.core.then
import org.http4k.routing.routes
import org.http4k.server.ServerConfig
import org.http4k.server.asServer

object Http4kBenchmarkServer {
    operator fun invoke(database: Database, addDateHeader: Boolean = true) =
        AddHeaders(addDateHeader)
            .then(
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
