import kotlinx.serialization.json.Json
import kotlinx.serialization.serializer

// copied from the `ktor` portion
// Optimized JSON instance with better performance settings
val json = Json {
    prettyPrint = false
    isLenient = true
    ignoreUnknownKeys = true
    coerceInputValues = true
}

object Serializers {
    val message = serializer<Message>()
    val world = serializer<World>()
    val worlds = serializer<List<World>>()
}
