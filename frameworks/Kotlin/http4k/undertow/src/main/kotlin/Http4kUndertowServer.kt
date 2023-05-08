import io.undertow.server.handlers.BlockingHandler
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kServer
import org.http4k.server.Http4kUndertowHttpHandler
import org.http4k.server.ServerConfig

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(CustomUndertow(9000))
}

private fun CustomUndertow(port: Int) = object : ServerConfig {
    override fun toServer(http: HttpHandler) = object : Http4kServer {
        val server = io.undertow.Undertow.builder()
            .addHttpListener(port, "0.0.0.0")
            .setHandler(BlockingHandler(Http4kUndertowHttpHandler(http)))
            .setWorkerThreads(64 * Runtime.getRuntime().availableProcessors())
            .build()

        override fun start() = apply { server.start() }

        override fun stop() = apply { server.stop() }

        override fun port() = port
    }
}
