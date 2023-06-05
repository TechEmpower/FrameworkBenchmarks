package benchmark

import benchmark.data.Fortune
import benchmark.data.TFBRepository
import com.fizzed.rocker.runtime.StringBuilderOutput
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
import java.util.concurrent.ThreadLocalRandom

object Benchmark

val logger = pelletLogger<Benchmark>()
val jsonEncoder = Json {
    prettyPrint = false
}

fun main() = runBlocking {
    val sharedRouter = httpRouter {
        get("/plaintext", ::handlePlain)
        get("/json", ::handleJson)
        get("/db", ::handleDb)
        get("/query", ::handleQuery)
        get("/updates", ::handleUpdates)
        get("/fortunes", ::handleFortunes)
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
        .jsonEntity(jsonEncoder, responseBody)
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}

private val repository = TFBRepository()

private suspend fun handleDb(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    val result = repository.fetchWorld()
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .jsonEntity(jsonEncoder, result)
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}

private suspend fun handleQuery(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    val rawQueries = context.firstQueryParameter("queries").getOrNull()
    val queries = (rawQueries?.toIntOrNull() ?: 1).coerceIn(1, 500)
    val worlds = (1 .. queries)
        .map {
            repository.fetchWorld()
        }
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .jsonEntity(jsonEncoder, worlds)
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}

private suspend fun handleUpdates(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    val rawQueries = context.firstQueryParameter("queries").getOrNull()
    val queries = (rawQueries?.toIntOrNull() ?: 1).coerceIn(1, 500)
    val worlds = (1 .. queries)
        .map {
            repository.fetchWorld()
        }
    val newWorlds = worlds.map {
        it.copy(
            randomNumber = ThreadLocalRandom.current().nextInt(1, 10001)
        )
    }
    repository.updateWorlds(newWorlds)
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .jsonEntity(jsonEncoder, newWorlds)
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}

private suspend fun handleFortunes(
    context: PelletHTTPRouteContext
): HTTPRouteResponse {
    val newFortune = Fortune(0, "Additional fortune added at request time.")
    val fortunes = repository.fetchFortunes().toMutableList()
    fortunes.add(newFortune)
    fortunes.sortBy { it.message }
    val template = views.fortunes.template(fortunes)
        .render(StringBuilderOutput.FACTORY)
        .toString()
    return HTTPRouteResponse.Builder()
        .statusCode(200)
        .entity(template, "text/html; charset=utf-8")
        .header("Server", "pellet")
        .header("Date", dateFormatter.format(Instant.now()))
        .build()
}