suspend fun main() =
    commonRunVertxServer(
        "Vert.x-Web Kotlinx with database (PostgreSQL)",
        {},
        { MainVerticle() }
    )
