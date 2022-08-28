import org.apache.hc.core5.http.impl.bootstrap.ServerBootstrap
import org.apache.hc.core5.http.io.SocketConfig
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kRequestHandler
import org.http4k.server.Http4kServer
import org.http4k.server.ServerConfig

fun main() {
    /**
     * we need a custom config here because of how virtual hosting is required in the TFB
     * environment. Normally we would just call the inbuilt ApacheServer(9000) function
     */
    val config = TfbApacheServer(9000)
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(config)
}

private class TfbApacheServer(val port: Int) : ServerConfig {
    override fun toServer(http: HttpHandler): Http4kServer = object : Http4kServer {
        val handler = Http4kRequestHandler(http)

        val server = ServerBootstrap.bootstrap()
            .setListenerPort(port)
            .setSocketConfig(SocketConfig.custom()
                .setTcpNoDelay(true)
                .setSoKeepAlive(true)
                .setSoReuseAddress(true)
                .setBacklogSize(1000)
                .build())
            .apply {
                register("*", handler) // standard hosting
                registerVirtual("10.0.0.1", "*", handler) // for virtual hosting
                setCanonicalHostName("tfb-server")
            }.create()

        override fun start() = apply { server.start() }

        override fun stop() = apply { server.stop() }

        override fun port(): Int = port
    }
}
