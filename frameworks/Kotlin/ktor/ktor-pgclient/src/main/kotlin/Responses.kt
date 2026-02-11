import io.ktor.http.*
import io.ktor.http.content.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.serialization.json.Json

// Optimized JSON instance with better performance settings
internal val json = Json {
    prettyPrint = false
    isLenient = true
    ignoreUnknownKeys = true
    coerceInputValues = true
}

internal suspend inline fun <reified E> RoutingCall.respondJson(response: E) {
    respond(TextContent(
        json.encodeToString(response),
        ContentType.Application.Json
    ))
}