suspend fun main() =
    commonRunVertxServer(
        "Vert.x-Web Kotlinx",
        {},
        { MainVerticle() }
    )
