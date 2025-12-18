import kotlinx.serialization.serializer

object Serializers {
    val message = serializer<Message>()
    val world = serializer<World>()
    val worlds = serializer<List<World>>()
}