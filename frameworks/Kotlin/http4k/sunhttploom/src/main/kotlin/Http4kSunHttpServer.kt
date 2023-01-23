import com.sun.net.httpserver.HttpServer
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kServer
import org.http4k.server.HttpExchangeHandler
import org.http4k.server.ServerConfig
import java.net.InetSocketAddress
import java.util.concurrent.Executors

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(SunHttpLoom(9000))
}

class SunHttpLoom(val port: Int = 8000) : ServerConfig {
    override val stopMode: ServerConfig.StopMode = ServerConfig.StopMode.Immediate

    override fun toServer(http: HttpHandler): Http4kServer = object : Http4kServer {
        override fun port(): Int = if (port > 0) port else server.address.port

        private val executor = Executors.newVirtualThreadPerTaskExecutor()
        private val server = HttpServer.create(InetSocketAddress(port), 1000)
        override fun start(): Http4kServer = apply {
            server.createContext("/", HttpExchangeHandler(http))
            server.executor = executor
            server.start()
        }

        override fun stop() = apply {
            server.stop(0)
        }
    }
}
