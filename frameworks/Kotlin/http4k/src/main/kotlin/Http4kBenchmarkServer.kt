import org.apache.commons.lang3.time.FastDateFormat.getInstance
import org.http4k.asByteBuffer
import org.http4k.core.Body
import org.http4k.core.ContentType
import org.http4k.core.Filter
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.then
import org.http4k.core.with
import org.http4k.format.Jackson
import org.http4k.format.Jackson.json
import org.http4k.lens.binary
import org.http4k.routing.by
import org.http4k.routing.routes
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.util.TimeZone.getTimeZone

object PlainTextRoute {
    private val preAllocatedHelloWorldText = "Hello, World!".asByteBuffer()

    private val plainTextBody = Body.binary(ContentType.TEXT_PLAIN).toLens()

    operator fun invoke() = GET to "/plaintext" by { Response(OK).with(plainTextBody of preAllocatedHelloWorldText) }
}

object JsonRoute {
    private val json = Jackson
    private val jsonBody = Body.json().toLens()

    operator fun invoke() =   GET to "/json" by { Response(OK).with(jsonBody of json.obj("message" to json.string("Hello, World!"))) }
}

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

    val app = routes(
        PlainTextRoute(),
        JsonRoute()
    )

    fun start(config: ServerConfig) = dateAndServer.then(app).asServer(config).start().block()
}