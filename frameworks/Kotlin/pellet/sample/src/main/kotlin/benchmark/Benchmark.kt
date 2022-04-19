package benchmark

import dev.pellet.logging.pelletLogger
import dev.pellet.server.PelletBuilder.httpRouter
import dev.pellet.server.PelletBuilder.pelletServer
import dev.pellet.server.PelletConnector
import dev.pellet.server.codec.mime.MediaType
import dev.pellet.server.responder.http.PelletHTTPRouteContext
import dev.pellet.server.routing.http.HTTPRouteResponse
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.Locale

object Benchmark

val logger = pelletLogger<Benchmark>()

fun main() = runBlocking {
    val sharedRouter = httpRouter {
        get("/plaintext", ::handlePlain)
        get("/json", ::handleJson)
    }
    val pellet = pelletServer {
        logRequests = false
        httpConnector {
            endpoint = PelletConnector.Endpoint(
                hostname = "0.0.0.0",
                port = 8080
            )
            router = sharedRouter
        }
    }
    pellet.start().join()
}

val dateFormatter = DateTimeFormatter
    .ofPattern("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
    .withZone(ZoneId.of("GMT"))

private suspend fun handlePlain(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .entity("Hello, World!", MediaType("text", "plain"))
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}

@kotlinx.serialization.Serializable
data class ResponseBody(
    val message: String
)

private suspend fun handleJson(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    val responseBody = ResponseBody(message = "Hello, World!")
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .jsonEntity(Json, responseBody)
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}
