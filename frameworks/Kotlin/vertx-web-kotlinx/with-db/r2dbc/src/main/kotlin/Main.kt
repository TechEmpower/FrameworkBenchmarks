suspend fun main() =
    commonRunVertxServer(
        "Vert.x-Web Kotlinx with R2DBC (and PostgreSQL)",
        {},
        { MainVerticle() }
    )
