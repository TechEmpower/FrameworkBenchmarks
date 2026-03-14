import kotlinx.serialization.Serializable
import kotlin.random.Random

@Serializable
class Message(val message: String)

@Serializable
data class World(val id: Int, val randomNumber: Int)

fun Random.nextIntBetween1And10000() =
    nextInt(1, 10001)

class Fortune(val id: Int, val message: String)
