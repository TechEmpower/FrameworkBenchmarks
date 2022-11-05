package http4k

import AddHeaders
import JsonRoute
import PlainTextRoute
import org.http4k.core.then
import org.http4k.routing.routes
import org.http4k.server.ApacheServer
import start

object Http4kGraalVMServer {
    operator fun invoke(addDateHeader: Boolean = true) =
        AddHeaders(addDateHeader)
            .then(
                routes(
                    JsonRoute(),
                    PlainTextRoute(),
                )
            )
}

fun main() {
    Http4kGraalVMServer().start(ApacheServer(9000))
}
