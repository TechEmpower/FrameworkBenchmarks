import org.eclipse.jetty.server.*
import org.eclipse.jetty.util.thread.ThreadPool
import org.http4k.core.HttpHandler
import org.http4k.server.Http4kServer
import org.http4k.server.ServerConfig
import org.http4k.server.ServerConfig.StopMode.Graceful
import org.http4k.server.http
import org.http4k.server.toJettyHandler
import java.time.Duration.ofSeconds
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.NANOSECONDS
import kotlin.Long.Companion.MAX_VALUE

class JettyLoom(private val port: Int) : ServerConfig {
    private val server = Server(LoomThreadPool())

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

class LoomThreadPool : ThreadPool {
    private val executorService = Executors.newVirtualThreadPerTaskExecutor()

    @Throws(InterruptedException::class)
    override fun join() {
        executorService.awaitTermination(MAX_VALUE, NANOSECONDS)
    }

    override fun getThreads() = 1

    override fun getIdleThreads() = 1

    override fun isLowOnThreads() = false

    override fun execute(command: Runnable) {
        executorService.submit(command)
    }
}