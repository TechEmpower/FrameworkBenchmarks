
import org.apache.commons.lang3.time.FastDateFormat.getInstance
import org.http4k.asByteBuffer
import org.http4k.contract.Root
import org.http4k.contract.Route
import org.http4k.contract.RouteModule
import org.http4k.contract.SimpleJson
import org.http4k.core.ContentType
import org.http4k.core.Filter
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.format.Jackson
import org.http4k.format.Jackson.json
import org.http4k.lens.Body
import org.http4k.server.ServerConfig
import org.http4k.server.asServer
import java.util.TimeZone.getTimeZone

object Http4kBenchmarkServer {
    private val json = Jackson
    private val preAllocatedHelloWorldText = "Hello, World!".asByteBuffer()
    private val dateFormat = getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", getTimeZone("GMT"))

    private val dateAndServer = Filter {
        next ->
        {
            next(it)
                .header("Server", "Example")
                .header("Date", dateFormat.format(System.currentTimeMillis()))
        }
    }

    private val jsonBody = Body.json().required()
    private val plainTextBody = Body.binary(ContentType.TEXT_PLAIN).required()

    private val module = RouteModule(Root, SimpleJson(json), dateAndServer)
        .withRoute(Route("plaintext").at(GET) / "plaintext" bind {
            Response(OK).with(plainTextBody to preAllocatedHelloWorldText)
        })
        .withRoute(Route("json").at(GET) / "json" bind {
            Response(OK).with(jsonBody to json.obj("message" to json.string("Hello, World!")))
        })

    fun start(config: ServerConfig) = module.toHttpHandler().asServer(config).start().block()
}