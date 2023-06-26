import org.eclipse.jetty.server.Server
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kServer
import org.http4k.server.ServerConfig
import org.http4k.server.ServerConfig.StopMode.Graceful
import org.http4k.server.http
import org.http4k.server.toJettyHandler
import java.time.Duration.ofSeconds
import java.util.concurrent.Executors

class JettyLoom(private val port: Int) : ServerConfig {
    private val server = Server(QueuedThreadPool().apply {
        virtualThreadsExecutor = Executors.newVirtualThreadPerTaskExecutor()
    })

    override val stopMode = Graceful(ofSeconds(5))

    override fun toServer(http: HttpHandler) = server.run {
        addConnector(http(port)(this))
        insertHandler(http.toJettyHandler(true))
        object : Http4kServer {
            override fun start(): Http4kServer = apply { server.start() }
            override fun stop(): Http4kServer = apply { server.stop() }
            override fun port(): Int = if (port > 0) port else server.uri.port
        }
    }
}
