import org.apache.http.config.SocketConfig
import org.apache.http.impl.bootstrap.ServerBootstrap
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kRequestHandler
import org.http4k.server.Http4kServer
import org.http4k.server.ServerConfig
import java.util.concurrent.TimeUnit

private const val PORT = 9000

fun main(args: Array<String>) {
    Http4kBenchmarkServer().start(object : ServerConfig {
        override fun toServer(httpHandler: HttpHandler): Http4kServer = object : Http4kServer {
            override fun port() = PORT

            private val server = ServerBootstrap.bootstrap()
                    .setListenerPort(PORT)
                    .setSocketConfig(SocketConfig.custom()
                            .setTcpNoDelay(true)
                            .setSoKeepAlive(true)
                            .setSoReuseAddress(true)
                            .setBacklogSize(512)
                            .build())
                    .registerHandler("*", Http4kRequestHandler(httpHandler))
                    .create()

            override fun start() = apply { server.start() }

            override fun stop() = server.shutdown(15, TimeUnit.SECONDS)
        }
    })
}