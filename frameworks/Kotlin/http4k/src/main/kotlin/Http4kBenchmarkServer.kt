import org.http4k.asByteBuffer
import org.http4k.contract.Root
import org.http4k.contract.Route
import org.http4k.contract.RouteModule
import org.http4k.contract.SimpleJson
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.format.Jackson
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.time.Instant.now
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

object Http4kBenchmarkServer {
    private val json = Jackson

    private val preAllocatedHelloWorldText = "Hello, World!".asByteBuffer()

    val module = RouteModule(Root, SimpleJson(json))
        .withRoute(Route("plaintext").at(GET) / "plaintext" bind {
            Response(OK)
                .header("Server", "Example")
                .header("Date", RFC_1123_DATE_TIME.format(now()))
                .body(preAllocatedHelloWorldText)
        })
        .withRoute(Route("json").at(GET) / "json" bind {
            Response(OK)
                .header("Server", "Example")
                .header("Date", RFC_1123_DATE_TIME.format(now()))
                .body(json.compact(json.obj("message" to json.string("Hello, World!"))))
        })

    fun start(config: ServerConfig) = module.toHttpHandler().asServer(config).start().block()
}