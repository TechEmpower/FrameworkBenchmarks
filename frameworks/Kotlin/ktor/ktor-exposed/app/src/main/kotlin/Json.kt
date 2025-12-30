import kotlinx.serialization.json.Json

// copied from the `ktor` portion
// Optimized JSON instance with better performance settings
internal val json = Json {
    prettyPrint = false
    isLenient = true
    ignoreUnknownKeys = true
    coerceInputValues = true
}
